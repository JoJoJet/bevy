use std::{cell::UnsafeCell, marker::PhantomData};

use bevy_ptr::UnsafeCellDeref;

use crate::prelude::World;

use super::{prototype::SystemPrototype, ParamSet, SystemMeta, SystemParam};

#[allow(unused_imports)] // Used in docs.
use crate::system::System;

/// Customizes the behavior of a [`CombinatorPrototype`].
///
/// # Examples
///
/// ```
/// use bevy_ecs::prelude::*;
/// use bevy_ecs::system::{CombinatorPrototype, SystemPrototype, Combine};
///
/// // A system combinator that performs an exclusive-or (XOR)
/// // operation on the output of two systems.
/// pub type Xor<A, B> = CombinatorPrototype<XorMarker, A, B>;
///
/// // This struct is used to customize the behavior of our combinator.
/// pub struct XorMarker;
///
/// impl Combine<(), bool, (), bool> for XorMarker
/// {
///     type In = ();
///     type Out = bool;
///
///     fn combine(
///         _input: Self::In,
///         a: impl FnOnce(()) -> bool,
///         b: impl FnOnce(()) -> bool,
///     ) -> Self::Out {
///         a(()) ^ b(())
///     }
/// }
///
/// # #[derive(Resource, PartialEq, Eq)] struct A(u32);
/// # #[derive(Resource, PartialEq, Eq)] struct B(u32);
/// # #[derive(Resource, Default)] struct RanFlag(bool);
/// # let mut world = World::new();
/// # world.init_resource::<RanFlag>();
/// #
/// # let mut app = Schedule::new();
/// app.add_system(my_system.run_if(Xor::new(
///     resource_equals(A(1)),
///     resource_equals(B(1)),
/// )));
/// # fn my_system(mut flag: ResMut<RanFlag>) { flag.0 = true; }
/// #
/// # world.insert_resource(A(0));
/// # world.insert_resource(B(0));
/// # app.run(&mut world);
/// # // Neither condition passes, so the system does not run.
/// # assert!(!world.resource::<RanFlag>().0);
/// #
/// # world.insert_resource(A(1));
/// # app.run(&mut world);
/// # // Only the first condition passes, so the system runs.
/// # assert!(world.resource::<RanFlag>().0);
/// # world.resource_mut::<RanFlag>().0 = false;
/// #
/// # world.insert_resource(B(1));
/// # app.run(&mut world);
/// # // Both conditions pass, so the system does not run.
/// # assert!(!world.resource::<RanFlag>().0);
/// #
/// # world.insert_resource(A(0));
/// # app.run(&mut world);
/// # // Only the second condition passes, so the system runs.
/// # assert!(world.resource::<RanFlag>().0);
/// # world.resource_mut::<RanFlag>().0 = false;
/// ```
pub trait Combine<AIn, AOut, BIn, BOut>: 'static {
    /// The [input](System::In) type for a [`CombinatorPrototype`].
    type In;

    /// The [output](System::Out) type for a [`CombinatorPrototype`].
    type Out;

    /// When used in a [`CombinatorPrototype`], this function customizes how
    /// the two composite systems are invoked and their outputs are combined.
    ///
    /// See the trait-level docs for [`Combine`] for an example implementation.
    fn combine(
        input: Self::In,
        a: impl FnOnce(AIn) -> AOut,
        b: impl FnOnce(BIn) -> BOut,
    ) -> Self::Out;
}

pub struct CombinatorPrototype<Func, A, B> {
    _marker: PhantomData<fn() -> Func>,
    a: A,
    b: B,
}

impl<Func, A, B> CombinatorPrototype<Func, A, B> {
    pub const fn new(a: A, b: B) -> Self {
        Self {
            _marker: PhantomData,
            a,
            b,
        }
    }
}

#[doc(hidden)]
pub struct IsCombinator;

impl<A, B, MarkerA, MarkerB, Func> SystemPrototype<(IsCombinator, Func, MarkerA, MarkerB)>
    for CombinatorPrototype<Func, A, B>
where
    MarkerA: 'static,
    MarkerB: 'static,
    A: SystemPrototype<MarkerA>,
    B: SystemPrototype<MarkerB>,
    Func: Combine<A::In, A::Out, B::In, B::Out>,
{
    const IS_EXCLUSIVE: bool = A::IS_EXCLUSIVE || B::IS_EXCLUSIVE;

    type In = Func::In;
    type Out = Func::Out;

    type Param = ParamSet<'static, 'static, (A::Param, B::Param)>;

    fn run_parallel(
        &mut self,
        input: Self::In,
        world: &World,
        (state_a, state_b): &mut <Self::Param as SystemParam>::State,
        system_meta: &SystemMeta,
    ) -> Self::Out {
        Func::combine(
            input,
            |input| self.a.run_parallel(input, world, state_a, system_meta),
            |input| self.b.run_parallel(input, world, state_b, system_meta),
        )
    }

    #[allow(clippy::undocumented_unsafe_blocks)]
    fn run_exclusive(
        &mut self,
        input: Self::In,
        world: &mut World,
        (state_a, state_b): &mut <Self::Param as SystemParam>::State,
        system_meta: &super::SystemMeta,
    ) -> Self::Out {
        // SAFETY: Converting `&mut T` -> `&UnsafeCell<T>`
        // is explicitly allowed in the docs for `UnsafeCell`.
        let world: &UnsafeCell<World> = unsafe { std::mem::transmute(world) };
        Func::combine(
            input,
            // SAFETY: Since these closures are `!Send + !Synd + !'static`, they can never
            // be called in parallel. Since mutable access to `world` only exists within
            // the scope of either closure, we can be sure they will never alias one another.
            |input| {
                self.a
                    .run_exclusive(input, unsafe { world.deref_mut() }, state_a, system_meta)
            },
            |input| {
                self.b
                    .run_exclusive(input, unsafe { world.deref_mut() }, state_b, system_meta)
            },
        )
    }
}
