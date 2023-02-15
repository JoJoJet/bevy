use crate::system::{BoxedSystem, CombinatorPrototype, Combine};

pub type BoxedCondition = BoxedSystem<(), bool>;

/// A system that determines if one or more scheduled systems should run.
///
/// Implemented for functions and closures that convert into [`System<In=(), Out=bool>`](crate::system::System)
/// with [read-only](crate::system::ReadOnlySystemParam) parameters.
pub trait Condition<Marker>: sealed::Condition<Marker> {
    /// Returns a new run condition that only returns `true`
    /// if both this one and the passed `and_then` return `true`.
    ///
    /// The returned run condition is short-circuiting, meaning
    /// `and_then` will only be invoked if `self` returns `true`.
    ///
    /// # Examples
    ///
    /// ```should_panic
    /// use bevy_ecs::prelude::*;
    ///
    /// #[derive(Resource, PartialEq)]
    /// struct R(u32);
    ///
    /// # let mut app = Schedule::new();
    /// # let mut world = World::new();
    /// # fn my_system() {}
    /// app.add_system(
    ///     // The `resource_equals` run condition will panic since we don't initialize `R`,
    ///     // just like if we used `Res<R>` in a system.
    ///     my_system.run_if(resource_equals(R(0))),
    /// );
    /// # app.run(&mut world);
    /// ```
    ///
    /// Use `.and_then()` to avoid checking the condition.
    ///
    /// ```
    /// # use bevy_ecs::prelude::*;
    /// # #[derive(Resource, PartialEq)]
    /// # struct R(u32);
    /// # let mut app = Schedule::new();
    /// # let mut world = World::new();
    /// # fn my_system() {}
    /// app.add_system(
    ///     // `resource_equals` will only get run if the resource `R` exists.
    ///     my_system.run_if(resource_exists::<R>().and_then(resource_equals(R(0)))),
    /// );
    /// # app.run(&mut world);
    /// ```
    ///
    /// Note that in this case, it's better to just use the run condition [`resource_exists_and_equals`].
    ///
    /// [`resource_exists_and_equals`]: common_conditions::resource_exists_and_equals
    fn and_then<P, C: Condition<P>>(self, and_then: C) -> AndThen<Self, C> {
        AndThen::new(self, and_then)
    }

    /// Returns a new run condition that returns `true`
    /// if either this one or the passed `or_else` return `true`.
    ///
    /// The returned run condition is short-circuiting, meaning
    /// `or_else` will only be invoked if `self` returns `false`.
    ///
    /// # Examples
    ///
    /// ```
    /// use bevy_ecs::prelude::*;
    ///
    /// #[derive(Resource, PartialEq)]
    /// struct A(u32);
    ///
    /// #[derive(Resource, PartialEq)]
    /// struct B(u32);
    ///
    /// # let mut app = Schedule::new();
    /// # let mut world = World::new();
    /// # #[derive(Resource)] struct C(bool);
    /// # fn my_system(mut c: ResMut<C>) { c.0 = true; }
    /// app.add_system(
    ///     // Only run the system if either `A` or `B` exist.
    ///     my_system.run_if(resource_exists::<A>().or_else(resource_exists::<B>())),
    /// );
    /// #
    /// # world.insert_resource(C(false));
    /// # app.run(&mut world);
    /// # assert!(!world.resource::<C>().0);
    /// #
    /// # world.insert_resource(A(0));
    /// # app.run(&mut world);
    /// # assert!(world.resource::<C>().0);
    /// #
    /// # world.remove_resource::<A>();
    /// # world.insert_resource(B(0));
    /// # world.insert_resource(C(false));
    /// # app.run(&mut world);
    /// # assert!(world.resource::<C>().0);
    /// ```
    fn or_else<P, C: Condition<P>>(self, or_else: C) -> OrElse<Self, C> {
        OrElse::new(self, or_else)
    }
}

impl<Marker, F> Condition<Marker> for F where F: sealed::Condition<Marker> {}

mod sealed {
    use crate::system::{ReadOnlySystemParam, SystemPrototype};

    pub trait Condition<Marker>: SystemPrototype<Marker, In = (), Out = bool> {}

    impl<Marker, F> Condition<Marker> for F
    where
        F: SystemPrototype<Marker, In = (), Out = bool>,
        F::Param: ReadOnlySystemParam,
    {
    }
}

pub mod common_conditions {
    use std::marker::PhantomData;

    use super::Condition;
    use crate::{
        schedule::{State, States},
        system::{ReadOnlySystemParam, Res, Resource, SystemMeta, SystemParam, SystemPrototype},
        world::World,
    };

    /// Generates a [`Condition`](super::Condition)-satisfying closure that returns `true`
    /// if the first time the condition is run and false every time after
    pub fn run_once() -> impl FnMut() -> bool {
        let mut has_run = false;
        move || {
            if !has_run {
                has_run = true;
                true
            } else {
                false
            }
        }
    }

    /// Generates a [`Condition`](super::Condition)-satisfying closure that returns `true`
    /// if the resource exists.
    pub fn resource_exists<T>() -> impl FnMut(Option<Res<T>>) -> bool
    where
        T: Resource,
    {
        move |res: Option<Res<T>>| res.is_some()
    }

    /// Generates a [`Condition`](super::Condition)-satisfying closure that returns `true`
    /// if the resource is equal to `value`.
    ///
    /// # Panics
    ///
    /// The condition will panic if the resource does not exist.
    pub fn resource_equals<T>(value: T) -> impl FnMut(Res<T>) -> bool
    where
        T: Resource + PartialEq,
    {
        move |res: Res<T>| *res == value
    }

    /// Generates a [`Condition`](super::Condition)-satisfying closure that returns `true`
    /// if the resource exists and is equal to `value`.
    ///
    /// The condition will return `false` if the resource does not exist.
    pub fn resource_exists_and_equals<T>(value: T) -> impl FnMut(Option<Res<T>>) -> bool
    where
        T: Resource + PartialEq,
    {
        move |res: Option<Res<T>>| match res {
            Some(res) => *res == value,
            None => false,
        }
    }

    /// Generates a [`Condition`](super::Condition)-satisfying closure that returns `true`
    /// if the state machine exists.
    pub fn state_exists<S: States>() -> impl FnMut(Option<Res<State<S>>>) -> bool {
        move |current_state: Option<Res<State<S>>>| current_state.is_some()
    }

    /// Generates a [`Condition`](super::Condition)-satisfying closure that returns `true`
    /// if the state machine is currently in `state`.
    ///
    /// # Panics
    ///
    /// The condition will panic if the resource does not exist.
    pub fn state_equals<S: States>(state: S) -> impl FnMut(Res<State<S>>) -> bool {
        move |current_state: Res<State<S>>| current_state.0 == state
    }

    /// Generates a [`Condition`](super::Condition)-satisfying closure that returns `true`
    /// if the state machine exists and is currently in `state`.
    ///
    /// The condition will return `false` if the state does not exist.
    pub fn state_exists_and_equals<S: States>(
        state: S,
    ) -> impl FnMut(Option<Res<State<S>>>) -> bool {
        move |current_state: Option<Res<State<S>>>| match current_state {
            Some(current_state) => current_state.0 == state,
            None => false,
        }
    }

    /// Generates a  [`Condition`](super::Condition) that inverses the result of passed one.
    ///
    /// # Examples
    ///
    /// ```
    /// use bevy_ecs::prelude::*;
    /// // Building a new schedule/app...
    /// let mut sched = Schedule::default();
    /// sched.add_system(
    ///         // This system will never run.
    ///         my_system.run_if(not(always_true))
    ///     )
    ///     // ...
    /// #   ;
    /// # let mut world = World::new();
    /// # sched.run(&mut world);
    ///
    /// // A condition that always returns true.
    /// fn always_true() -> bool {
    ///    true
    /// }
    /// #
    /// # fn my_system() { unreachable!() }
    /// ```
    pub fn not<Marker: 'static, Param>(
        condition: impl Condition<Marker, Param = Param>,
    ) -> impl Condition<(), Param = Param>
    where
        Param: ReadOnlySystemParam,
    {
        struct Not<Marker, T>
        where
            T: SystemPrototype<Marker>,
        {
            inner: T,
            _marker: PhantomData<fn() -> Marker>,
        }

        impl<Marker, T> SystemPrototype<()> for Not<Marker, T>
        where
            Marker: 'static,
            T: SystemPrototype<Marker>,
            T::Out: std::ops::Not,
        {
            const IS_EXCLUSIVE: bool = T::IS_EXCLUSIVE;

            type In = T::In;
            type Out = <T::Out as std::ops::Not>::Output;

            type Param = T::Param;

            fn run_parallel(
                &mut self,
                input: Self::In,
                world: &World,
                state: &mut <Self::Param as SystemParam>::State,
                system_meta: &SystemMeta,
            ) -> Self::Out {
                !self.inner.run_parallel(input, world, state, system_meta)
            }

            fn run_exclusive(
                &mut self,
                input: Self::In,
                world: &mut World,
                state: &mut <T::Param as SystemParam>::State,
                system_meta: &crate::system::SystemMeta,
            ) -> Self::Out {
                !self.inner.run_exclusive(input, world, state, system_meta)
            }
        }

        Not {
            inner: condition,
            _marker: PhantomData,
        }
    }
}

/// Combines the outputs of two systems using the `&&` operator.
pub type AndThen<A, B> = CombinatorPrototype<AndThenMarker, A, B>;

/// Combines the outputs of two systems using the `||` operator.
pub type OrElse<A, B> = CombinatorPrototype<OrElseMarker, A, B>;

#[doc(hidden)]
pub struct AndThenMarker;

impl<In> Combine<In, bool, In, bool> for AndThenMarker
where
    In: Copy,
{
    type In = In;
    type Out = bool;

    fn combine(
        input: Self::In,
        a: impl FnOnce(In) -> bool,
        b: impl FnOnce(In) -> bool,
    ) -> Self::Out {
        a(input) && b(input)
    }
}

#[doc(hidden)]
pub struct OrElseMarker;

impl<In> Combine<In, bool, In, bool> for OrElseMarker
where
    In: Copy,
{
    type In = In;
    type Out = bool;

    fn combine(
        input: Self::In,
        a: impl FnOnce(In) -> bool,
        b: impl FnOnce(In) -> bool,
    ) -> Self::Out {
        a(input) || b(input)
    }
}
