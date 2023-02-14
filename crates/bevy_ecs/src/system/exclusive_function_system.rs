use crate::{
    system::{ExclusiveSystemParam, ExclusiveSystemParamItem, In, InputMarker},
    world::World,
};
use bevy_ecs_macros::all_tuples;

pub struct IsExclusiveFunctionSystem;

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
