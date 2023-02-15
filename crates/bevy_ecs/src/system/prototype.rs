use std::marker::PhantomData;

use crate::{
    archetype::{ArchetypeGeneration, ArchetypeId},
    change_detection::MAX_CHANGE_AGE,
    prelude::FromWorld,
    world::{World, WorldId},
};

use super::{
    check_system_change_tick, ExclusiveSystemParam, ExclusiveSystemParamFunction, IntoSystem,
    IsExclusiveFunctionSystem, IsFunctionSystem, Local, ReadOnlySystem, ReadOnlySystemParam,
    System, SystemMeta, SystemParam, SystemParamFunction,
};

pub trait SystemPrototype<Marker>: Sized + Send + Sync + 'static {
    const IS_EXCLUSIVE: bool;

    type In;
    type Out;

    type Param: SystemParam;

    fn update_archetype_component_access(
        &mut self,
        state: &mut <Self::Param as SystemParam>::State,
        system_meta: &mut SystemMeta,
        world: &World,
    );

    fn run_parallel(
        &mut self,
        input: Self::In,
        world: &World,
        state: &mut <Self::Param as SystemParam>::State,
        system_meta: &SystemMeta,
    ) -> Self::Out;
    fn run_exclusive(
        &mut self,
        input: Self::In,
        world: &mut World,
        state: &mut <Self::Param as SystemParam>::State,
        system_meta: &mut SystemMeta,
    ) -> Self::Out;

    fn check_change_tick(
        &mut self,
        state: &mut <Self::Param as SystemParam>::State,
        change_tick: u32,
    );

    fn get_last_change_tick(&self, state: &<Self::Param as SystemParam>::State) -> u32;

    fn set_last_change_tick(
        &mut self,
        state: &mut <Self::Param as SystemParam>::State,
        change_tick: u32,
    );
}

#[doc(hidden)]
pub struct LastChangeTick(u32);

impl FromWorld for LastChangeTick {
    fn from_world(world: &mut World) -> Self {
        let tick = world.change_tick().wrapping_sub(MAX_CHANGE_AGE);
        Self(tick)
    }
}

impl<Marker, F> SystemPrototype<(IsFunctionSystem, Marker)> for F
where
    F: SystemParamFunction<Marker>,
{
    const IS_EXCLUSIVE: bool = false;

    type In = F::In;
    type Out = F::Out;

    type Param = (
        Local<'static, LastChangeTick>,
        Local<'static, ArchetypeGeneration>,
        F::Param,
    );

    fn update_archetype_component_access(
        &mut self,
        state: &mut <Self::Param as SystemParam>::State,
        system_meta: &mut SystemMeta,
        world: &World,
    ) {
        let archetype_generation = state.1.get();
        let archetypes = world.archetypes();
        let new_generation = archetypes.generation();
        let old_generation = std::mem::replace(archetype_generation, new_generation);
        let archetype_index_range = old_generation.value()..new_generation.value();

        for archetype_index in archetype_index_range {
            F::Param::new_archetype(
                &mut state.2,
                &archetypes[ArchetypeId::new(archetype_index)],
                system_meta,
            );
        }
    }

    fn run_parallel(
        &mut self,
        input: Self::In,
        world: &World,
        (last_change_tick, _, state): &mut <Self::Param as SystemParam>::State,
        system_meta: &SystemMeta,
    ) -> Self::Out {
        let last_change_tick = &mut last_change_tick.get().0;
        let change_tick = world.read_change_tick();
        // SAFETY: shut up clippy
        let params = unsafe {
            F::Param::get_param(state, system_meta, world, change_tick, *last_change_tick)
        };
        let output = self.run(input, params);
        *last_change_tick = change_tick;
        output
    }

    fn run_exclusive(
        &mut self,
        input: Self::In,
        world: &mut World,
        state: &mut <Self::Param as SystemParam>::State,
        system_meta: &mut SystemMeta,
    ) -> Self::Out {
        self.update_archetype_component_access(state, system_meta, world);
        self.run_parallel(input, world, state, system_meta)
    }

    fn check_change_tick(
        &mut self,
        (last_change_tick, ..): &mut <Self::Param as SystemParam>::State,
        change_tick: u32,
    ) {
        check_system_change_tick(
            &mut last_change_tick.get().0,
            change_tick,
            std::any::type_name::<F>(),
        );
    }

    fn get_last_change_tick(
        &self,
        (last_change_tick, ..): &<Self::Param as SystemParam>::State,
    ) -> u32 {
        last_change_tick.read().0
    }

    fn set_last_change_tick(
        &mut self,
        (last_change_tick, ..): &mut <Self::Param as SystemParam>::State,
        change_tick: u32,
    ) {
        last_change_tick.get().0 = change_tick;
    }
}

impl<Marker, F> SystemPrototype<(IsExclusiveFunctionSystem, Marker)> for F
where
    F: ExclusiveSystemParamFunction<Marker>,
{
    const IS_EXCLUSIVE: bool = true;

    type In = F::In;
    type Out = F::Out;

    type Param = (Local<'static, LastChangeTick>, WithState<F::Param>);

    fn update_archetype_component_access(
        &mut self,
        _state: &mut <Self::Param as SystemParam>::State,
        _system_meta: &mut SystemMeta,
        _world: &World,
    ) {
    }

    fn run_parallel(
        &mut self,
        _input: Self::In,
        _world: &World,
        _state: &mut <Self::Param as SystemParam>::State,
        _system_meta: &SystemMeta,
    ) -> Self::Out {
        panic!("Cannot run exclusive systems with a shared World reference");
    }

    fn run_exclusive(
        &mut self,
        input: Self::In,
        world: &mut World,
        (last_change_tick, state): &mut <Self::Param as SystemParam>::State,
        system_meta: &mut SystemMeta,
    ) -> Self::Out {
        let last_change_tick = &mut last_change_tick.get().0;
        let saved_last_tick = world.last_change_tick;
        world.last_change_tick = *last_change_tick;

        let param = F::Param::get_param(state, system_meta);
        let output = self.run(world, input, param);

        let change_tick = world.change_tick.get_mut();
        *last_change_tick = *change_tick;
        *change_tick = change_tick.wrapping_add(1);
        world.last_change_tick = saved_last_tick;

        output
    }

    fn check_change_tick(
        &mut self,
        (last_change_tick, _): &mut <Self::Param as SystemParam>::State,
        change_tick: u32,
    ) {
        check_system_change_tick(
            &mut last_change_tick.get().0,
            change_tick,
            std::any::type_name::<F>(),
        );
    }

    fn get_last_change_tick(
        &self,
        (last_change_tick, _): &<Self::Param as SystemParam>::State,
    ) -> u32 {
        last_change_tick.read().0
    }

    fn set_last_change_tick(
        &mut self,
        (last_change_tick, _): &mut <Self::Param as SystemParam>::State,
        change_tick: u32,
    ) {
        last_change_tick.get().0 = change_tick;
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
        _last_change_tick: u32,
        _change_tick: u32,
    ) -> Self::Item<'world, 'state> {
        Self(PhantomData)
    }
}

pub struct MapPrototype<Marker, T, F, Out>
where
    T: SystemPrototype<Marker>,
    F: FnMut(T::Out) -> Out,
{
    inner: T,
    func: F,
    _marker: PhantomData<fn() -> (Marker, Out)>,
}

impl<Marker, T, F, Out> MapPrototype<Marker, T, F, Out>
where
    T: SystemPrototype<Marker>,
    F: FnMut(T::Out) -> Out,
{
    pub fn new(inner: T, func: F) -> Self {
        Self {
            inner,
            func,
            _marker: PhantomData,
        }
    }
}

impl<Marker, T, F, Out> SystemPrototype<()> for MapPrototype<Marker, T, F, Out>
where
    Marker: 'static,
    T: SystemPrototype<Marker>,
    F: FnMut(T::Out) -> Out + Send + Sync + 'static,
    Out: 'static,
{
    type In = T::In;
    type Out = Out;

    type Param = T::Param;

    const IS_EXCLUSIVE: bool = T::IS_EXCLUSIVE;

    fn update_archetype_component_access(
        &mut self,
        state: &mut <Self::Param as SystemParam>::State,
        system_meta: &mut SystemMeta,
        world: &World,
    ) {
        self.inner
            .update_archetype_component_access(state, system_meta, world);
    }

    fn run_parallel(
        &mut self,
        input: Self::In,
        world: &World,
        state: &mut <Self::Param as SystemParam>::State,
        system_meta: &SystemMeta,
    ) -> Self::Out {
        let val = self.inner.run_parallel(input, world, state, system_meta);
        (self.func)(val)
    }

    fn run_exclusive(
        &mut self,
        input: Self::In,
        world: &mut World,
        state: &mut <Self::Param as SystemParam>::State,
        system_meta: &mut SystemMeta,
    ) -> Self::Out {
        let val = self.inner.run_exclusive(input, world, state, system_meta);
        (self.func)(val)
    }

    fn check_change_tick(
        &mut self,
        state: &mut <Self::Param as SystemParam>::State,
        change_tick: u32,
    ) {
        self.inner.check_change_tick(state, change_tick);
    }

    fn get_last_change_tick(&self, state: &<Self::Param as SystemParam>::State) -> u32 {
        self.inner.get_last_change_tick(state)
    }

    fn set_last_change_tick(
        &mut self,
        state: &mut <Self::Param as SystemParam>::State,
        change_tick: u32,
    ) {
        self.inner.set_last_change_tick(state, change_tick);
    }
}

pub struct PrototypeSystem<Marker, T>
where
    T: SystemPrototype<Marker>,
{
    prototype: T,
    param_state: Option<<T::Param as SystemParam>::State>,
    system_meta: SystemMeta,
    world_id: Option<WorldId>,
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
        self.prototype.run_parallel(
            input,
            world,
            self.param_state.as_mut().expect(PARAM_MESSAGE),
            &self.system_meta,
        )
    }

    fn run(&mut self, input: Self::In, world: &mut World) -> Self::Out {
        assert!(self.world_id == Some(world.id()), "Encountered a mismatched World. A System cannot be used with Worlds other than the one it was initialized with.");
        self.prototype.run_exclusive(
            input,
            world,
            self.param_state.as_mut().expect(PARAM_MESSAGE),
            &mut self.system_meta,
        )
    }

    fn apply_buffers(&mut self, world: &mut World) {
        let param_state = self.param_state.as_mut().expect(PARAM_MESSAGE);
        T::Param::apply(param_state, &self.system_meta, world);
    }

    fn initialize(&mut self, world: &mut World) {
        self.world_id = Some(world.id());
        self.param_state = Some(T::Param::init_state(world, &mut self.system_meta));
    }

    fn update_archetype_component_access(&mut self, world: &World) {
        assert!(self.world_id == Some(world.id()), "Encountered a mismatched World. A System cannot be used with Worlds other than the one it was initialized with.");

        self.prototype.update_archetype_component_access(
            self.param_state.as_mut().unwrap(),
            &mut self.system_meta,
            world,
        );
    }

    fn check_change_tick(&mut self, change_tick: u32) {
        self.prototype
            .check_change_tick(self.param_state.as_mut().unwrap(), change_tick);
    }

    fn get_last_change_tick(&self) -> u32 {
        self.prototype
            .get_last_change_tick(self.param_state.as_ref().unwrap())
    }

    fn set_last_change_tick(&mut self, last_change_tick: u32) {
        self.prototype
            .set_last_change_tick(self.param_state.as_mut().unwrap(), last_change_tick);
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

pub struct IsPrototypeSystem;

impl<Marker, T> IntoSystem<T::In, T::Out, (IsPrototypeSystem, Marker)> for T
where
    Marker: 'static,
    T: SystemPrototype<Marker>,
{
    type System = PrototypeSystem<Marker, T>;
    fn into_system(prototype: Self) -> Self::System {
        PrototypeSystem {
            prototype,
            param_state: None,
            system_meta: SystemMeta::new::<T>(),
            world_id: None,
            marker: PhantomData,
        }
    }
}
