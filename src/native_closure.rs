use std::any::Any;
use crate::interpreter::Return;
use crate::expression::Args;
use crate::errors::Result;

pub use implementation::NativeClosure;

#[cfg(feature = "thread-safe")]
mod implementation {
    use super::*;

    pub struct NativeClosure {
        function: fn(Args, &[Box<dyn Any + Send + Sync>]) -> Result<Return>,
        variables: Vec<Box<dyn Any + Send + Sync>>,
    }

    impl NativeClosure {
        pub fn new(
            variables: Vec<Box<dyn Any + Send + Sync>>,
            function: fn(Args, &[Box<dyn Any + Send + Sync>]) -> Result<Return>,
        ) -> Self {
            NativeClosure {
                function,
                variables,
            }
        }

        pub fn invoke(&self, args: Args) -> Result<Return> {
            (self.function)(args, &self.variables)
        }
    }
}

#[cfg(not(feature = "thread-safe"))]
mod implementation {
    use super::*;

    pub struct NativeClosure {
        function: fn(Args, &[Box<dyn Any>]) -> Result<Return>,
        variables: Vec<Box<dyn Any>>,
    }

    impl NativeClosure {
        pub fn new(
            variables: Vec<Box<dyn Any>>,
            function: fn(Args, &[Box<dyn Any>]) -> Result<Return>,
        ) -> Self {
            NativeClosure {
                function,
                variables,
            }
        }

        pub fn invoke(&self, args: Args) -> Result<Return> {
            (self.function)(args, &self.variables)
        }
    }
}