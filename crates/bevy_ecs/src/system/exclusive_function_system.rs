use std::marker::PhantomData;

use crate::{
    system::{ExclusiveSystemParam, ExclusiveSystemParamItem, In, InputMarker},
    world::World,
};
use bevy_ecs_macros::all_tuples;

#[allow(unused_imports)] // Used in docs.
use crate::system::System;

use super::{
    check_system_change_tick, LastChangeTick, Local, SystemMeta, SystemParam, SystemPrototype,
};

pub struct IsExclusiveFunctionSystem;

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

/// A trait implemented for all exclusive system functions that can be used as [`System`]s.
///
/// This trait can be useful for making your own systems which accept other systems,
/// sometimes called higher order systems.
pub trait ExclusiveSystemParamFunction<Marker>: Send + Sync + 'static {
    /// The input type to this system. See [`System::In`].
    type In;

    /// The return type of this system. See [`System::Out`].
    type Out;

    /// The [`ExclusiveSystemParam`]/s defined by this system's `fn` parameters.
    type Param: ExclusiveSystemParam;

    /// Executes this system once. See [`System::run`].
    fn run(
        &mut self,
        world: &mut World,
        input: Self::In,
        param_value: ExclusiveSystemParamItem<Self::Param>,
    ) -> Self::Out;
}

macro_rules! impl_exclusive_system_function {
    ($($param: ident),*) => {
        #[allow(non_snake_case)]
        impl<Out, Func: Send + Sync + 'static, $($param: ExclusiveSystemParam),*> ExclusiveSystemParamFunction<((), Out, $($param,)*)> for Func
        where
        for <'a> &'a mut Func:
                FnMut(&mut World, $($param),*) -> Out +
                FnMut(&mut World, $(ExclusiveSystemParamItem<$param>),*) -> Out,
            Out: 'static,
        {
            type In = ();
            type Out = Out;
            type Param = ($($param,)*);
            #[inline]
            fn run(&mut self, world: &mut World, _in: (), param_value: ExclusiveSystemParamItem< ($($param,)*)>) -> Out {
                // Yes, this is strange, but `rustc` fails to compile this impl
                // without using this function. It fails to recognise that `func`
                // is a function, potentially because of the multiple impls of `FnMut`
                #[allow(clippy::too_many_arguments)]
                fn call_inner<Out, $($param,)*>(
                    mut f: impl FnMut(&mut World, $($param,)*) -> Out,
                    world: &mut World,
                    $($param: $param,)*
                ) -> Out {
                    f(world, $($param,)*)
                }
                let ($($param,)*) = param_value;
                call_inner(self, world, $($param),*)
            }
        }
        #[allow(non_snake_case)]
        impl<Input, Out, Func: Send + Sync + 'static, $($param: ExclusiveSystemParam),*> ExclusiveSystemParamFunction<(Input, Out, $($param,)* InputMarker)> for Func
        where
        for <'a> &'a mut Func:
                FnMut(In<Input>, &mut World, $($param),*) -> Out +
                FnMut(In<Input>, &mut World, $(ExclusiveSystemParamItem<$param>),*) -> Out,
            Out: 'static,
        {
            type In = Input;
            type Out = Out;
            type Param = ($($param,)*);
            #[inline]
            fn run(&mut self, world: &mut World, input: Input, param_value: ExclusiveSystemParamItem< ($($param,)*)>) -> Out {
                // Yes, this is strange, but `rustc` fails to compile this impl
                // without using this function. It fails to recognise that `func`
                // is a function, potentially because of the multiple impls of `FnMut`
                #[allow(clippy::too_many_arguments)]
                fn call_inner<Input, Out, $($param,)*>(
                    mut f: impl FnMut(In<Input>, &mut World, $($param,)*) -> Out,
                    input: Input,
                    world: &mut World,
                    $($param: $param,)*
                ) -> Out {
                    f(In(input), world, $($param,)*)
                }
                let ($($param,)*) = param_value;
                call_inner(self, input, world, $($param),*)
            }
        }
    };
}
// Note that we rely on the highest impl to be <= the highest order of the tuple impls
// of `SystemParam` created.
all_tuples!(impl_exclusive_system_function, 0, 16, F);
