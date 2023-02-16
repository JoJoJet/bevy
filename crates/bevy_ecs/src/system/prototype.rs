use std::marker::PhantomData;

use crate::{
    change_detection::MAX_CHANGE_AGE,
    world::{World, WorldId},
};

use super::{
    check_system_change_tick, IntoSystem, ReadOnlySystem, ReadOnlySystemParam, System, SystemMeta,
    SystemParam,
};

/// Types that define a [`System`], and can be converted into one.
/// This is implemented for [regular system functions] as well as
/// [exclusive system functions].
///
/// Types that have already been converted into a `System` do not
/// implement this trait -- this is the main difference between
/// [`IntoSystem`] and [`SystemPrototype`].
/// * `SystemPrototype` is implemented for types that can be converted
/// to a system, excluding types that are already systems.
/// * `IntoSystem` is implemented for any types that can be converted
/// to a system, including systems and system prototypes.
///
/// [regular system functions]: crate::system::SystemParamFunction
/// [exclusive system functions]: crate::system::ExclusiveSystemParamFunction
pub trait SystemPrototype<Marker>: Sized + Send + Sync + 'static {
    /// If `true`, then this prototype describes an exclusive system.
    /// Otherwise, this prototype describes a system that can run in parallel with other systems.
    const IS_EXCLUSIVE: bool;

    /// The input type to this prototype. See [`System::In`].
    type In;

    /// The output type for this prototype. See [`System::Out`].
    type Out;

    /// The [`SystemParam`] describing the [`World`] accesses used by this prototype.
    type Param: SystemParam;

    /// Updates this system's archetype component [`Access`](crate::query::Access).
    fn update_archetype_component_access(
        &mut self,
        state: &mut <Self::Param as SystemParam>::State,
        system_meta: &mut SystemMeta,
        world: &World,
    );

    /// Executes the system once, potentially in parallel with other systems.
    ///
    /// # Safety
    ///
    /// It must be safe to call [`Self::Param::get_param`] with `world`.
    /// This means that no other systems may conflict with any of the
    /// [`World`] data accesses used by `Self::Param`.
    ///
    /// [`Self::Param::get_param`]: SystemParam::get_param
    unsafe fn run_parallel(
        &mut self,
        input: Self::In,
        world: &World,
        state: &mut <Self::Param as SystemParam>::State,
        system_meta: &mut SystemMeta,
    ) -> Self::Out;

    /// Executes the system once, with exclusive access to the [`World`].
    fn run_exclusive(
        &mut self,
        input: Self::In,
        world: &mut World,
        state: &mut <Self::Param as SystemParam>::State,
        system_meta: &mut SystemMeta,
    ) -> Self::Out;
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
        // Exclusive systems have have access to non-send resources,
        // so the executor must run them on the main thread.
        // For parallel systems, it depends on the accesses registered.
        !T::IS_EXCLUSIVE && self.system_meta.is_send
    }

    fn is_exclusive(&self) -> bool {
        T::IS_EXCLUSIVE
    }

    unsafe fn run_unsafe(&mut self, input: Self::In, world: &World) -> Self::Out {
        // SAFETY: We register all of the world accesses used by `Self::Param`,
        // so the caller will guarantee that there are no data access conflicts.
        self.prototype.run_parallel(
            input,
            world,
            self.param_state.as_mut().expect(PARAM_MESSAGE),
            &mut self.system_meta,
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
        self.system_meta.last_change_tick = world.change_tick().wrapping_sub(MAX_CHANGE_AGE);
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
        check_system_change_tick(
            &mut self.system_meta.last_change_tick,
            change_tick,
            &self.system_meta.name,
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
