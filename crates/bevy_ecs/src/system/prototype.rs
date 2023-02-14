use std::marker::PhantomData;

use crate::{
    archetype::{ArchetypeGeneration, ArchetypeId},
    change_detection::MAX_CHANGE_AGE,
    world::{World, WorldId},
};

use super::{
    check_system_change_tick, ExclusiveSystemParam, ExclusiveSystemParamFunction,
    IsExclusiveFunctionSystem, IsFunctionSystem, ReadOnlySystem, ReadOnlySystemParam, System,
    SystemMeta, SystemParam, SystemParamFunction, SystemParamItem,
};

pub trait SystemPrototype<Marker>: Send + Sync + 'static {
    const IS_EXCLUSIVE: bool;

    type In;
    type Out;

    type Param: SystemParam<State = Self::State>;
    type State: Send + Sync + 'static;

    fn run_parallel(&mut self, input: Self::In, param: SystemParamItem<Self::Param>) -> Self::Out;
    fn run_exclusive(
        &mut self,
        input: Self::In,
        world: &mut World,
        state: &mut Self::State,
        system_meta: &SystemMeta,
        change_tick: u32,
    ) -> Self::Out;
}

impl<Marker, F> SystemPrototype<(IsFunctionSystem, Marker)> for F
where
    F: SystemParamFunction<Marker>,
{
    const IS_EXCLUSIVE: bool = false;

    type In = F::In;
    type Out = F::Out;

    type Param = F::Param;
    type State = <F::Param as SystemParam>::State;

    fn run_parallel(&mut self, input: Self::In, param: SystemParamItem<Self::Param>) -> Self::Out {
        self.run(input, param)
    }

    fn run_exclusive(
        &mut self,
        input: Self::In,
        world: &mut World,
        state: &mut Self::State,
        system_meta: &SystemMeta,
        change_tick: u32,
    ) -> Self::Out {
        // SAFETY: shut up clippy
        let params = unsafe { F::Param::get_param(state, system_meta, world, change_tick) };
        self.run(input, params)
    }
}

impl<Marker, F> SystemPrototype<(IsExclusiveFunctionSystem, Marker)> for F
where
    F: ExclusiveSystemParamFunction<Marker>,
{
    const IS_EXCLUSIVE: bool = true;

    type In = F::In;
    type Out = F::Out;

    type Param = WithState<F::Param>;
    type State = <F::Param as ExclusiveSystemParam>::State;

    fn run_parallel(
        &mut self,
        _input: Self::In,
        _param: SystemParamItem<Self::Param>,
    ) -> Self::Out {
        panic!("Cannot run exclusive systems with a shared World reference");
    }

    fn run_exclusive(
        &mut self,
        input: Self::In,
        world: &mut World,
        state: &mut Self::State,
        system_meta: &SystemMeta,
        _change_tick: u32,
    ) -> Self::Out {
        let param = F::Param::get_param(state, system_meta);
        self.run(world, input, param)
    }
}

pub struct WithState<T>(PhantomData<T>);

// SAFETY: No world access.
unsafe impl<T: ExclusiveSystemParam> SystemParam for WithState<T> {
    type State = T::State;
    type Item<'world, 'state> = Self;

    fn init_state(world: &mut World, system_meta: &mut SystemMeta) -> Self::State {
        T::init(world, system_meta)
    }

    unsafe fn get_param<'world, 'state>(
        _state: &'state mut Self::State,
        _system_meta: &SystemMeta,
        _world: &'world World,
        _change_tick: u32,
    ) -> Self::Item<'world, 'state> {
        Self(PhantomData)
    }
}

pub struct PrototypeSystem<Marker, T>
where
    T: SystemPrototype<Marker>,
{
    prototype: T,
    param_state: Option<T::State>,
    system_meta: SystemMeta,
    world_id: Option<WorldId>,
    archetype_generation: ArchetypeGeneration,
    // NOTE: PhantomData<fn()-> T> gives this safe Send/Sync impls
    marker: PhantomData<fn() -> Marker>,
}

/// Message shown when a system isn't initialised
// When lines get too long, rustfmt can sometimes refuse to format them.
// Work around this by storing the message separately.
const PARAM_MESSAGE: &str = "System's param_state was not found. Did you forget to initialize this system before running it?";

impl<Marker, T> System for PrototypeSystem<Marker, T>
where
    Marker: 'static,
    T: SystemPrototype<Marker>,
{
    type In = T::In;

    type Out = T::Out;

    fn name(&self) -> std::borrow::Cow<'static, str> {
        self.system_meta.name.clone()
    }

    fn type_id(&self) -> std::any::TypeId {
        std::any::TypeId::of::<T>()
    }

    fn component_access(&self) -> &crate::query::Access<crate::component::ComponentId> {
        self.system_meta.component_access_set.combined_access()
    }

    fn archetype_component_access(
        &self,
    ) -> &crate::query::Access<crate::archetype::ArchetypeComponentId> {
        &self.system_meta.archetype_component_access
    }

    fn is_send(&self) -> bool {
        self.system_meta.is_send
    }

    fn is_exclusive(&self) -> bool {
        T::IS_EXCLUSIVE
    }

    unsafe fn run_unsafe(&mut self, input: Self::In, world: &World) -> Self::Out {
        let change_tick = world.increment_change_tick();

        // Safety:
        // We update the archetype component access correctly based on `Param`'s requirements
        // in `update_archetype_component_access`.
        // Our caller upholds the requirements.
        let params = T::Param::get_param(
            self.param_state.as_mut().expect(PARAM_MESSAGE),
            &self.system_meta,
            world,
            change_tick,
        );
        let out = self.prototype.run_parallel(input, params);
        self.system_meta.last_change_tick = change_tick;
        out
    }

    fn apply_buffers(&mut self, world: &mut World) {
        let param_state = self.param_state.as_mut().expect(PARAM_MESSAGE);
        T::Param::apply(param_state, &self.system_meta, world);
    }

    fn initialize(&mut self, world: &mut World) {
        self.world_id = Some(world.id());
        self.system_meta.last_change_tick = world.change_tick().wrapping_sub(MAX_CHANGE_AGE);
        self.param_state = Some(T::Param::init_state(world, &mut self.system_meta));
    }

    fn update_archetype_component_access(&mut self, world: &World) {
        assert!(self.world_id == Some(world.id()), "Encountered a mismatched World. A System cannot be used with Worlds other than the one it was initialized with.");

        if T::IS_EXCLUSIVE {
            return;
        }

        let archetypes = world.archetypes();
        let new_generation = archetypes.generation();
        let old_generation = std::mem::replace(&mut self.archetype_generation, new_generation);
        let archetype_index_range = old_generation.value()..new_generation.value();

        for archetype_index in archetype_index_range {
            let param_state = self.param_state.as_mut().unwrap();
            T::Param::new_archetype(
                param_state,
                &archetypes[ArchetypeId::new(archetype_index)],
                &mut self.system_meta,
            );
        }
    }

    fn check_change_tick(&mut self, change_tick: u32) {
        check_system_change_tick(
            &mut self.system_meta.last_change_tick,
            change_tick,
            self.system_meta.name.as_ref(),
        );
    }

    fn get_last_change_tick(&self) -> u32 {
        self.system_meta.last_change_tick
    }

    fn set_last_change_tick(&mut self, last_change_tick: u32) {
        self.system_meta.last_change_tick = last_change_tick;
    }

    fn default_system_sets(&self) -> Vec<Box<dyn crate::schedule::SystemSet>> {
        let set = crate::schedule::SystemTypeSet::<T>::new();
        vec![Box::new(set)]
    }
}

/// SAFETY: `T`'s param is `ReadOnlySystemParam`, so this system will only read from the world.
unsafe impl<Marker, T> ReadOnlySystem for PrototypeSystem<Marker, T>
where
    Marker: 'static,
    T: SystemPrototype<Marker>,
    T::Param: ReadOnlySystemParam,
{
}
