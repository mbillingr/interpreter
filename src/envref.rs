pub use env_ref_impl::{EnvRef, EnvWeak};

#[cfg(feature = "thread-safe")]
mod env_ref_impl {
    use crate::environment::Environment;
    use crate::expression::{Ref, Weak};
    use std::sync::{self, RwLock};

    #[derive(Clone)]
    pub struct EnvRef {
        env: Ref<RwLock<Environment>>,
        ptr: *const Environment,
    }

    pub type EnvTmpRef<'a> = sync::RwLockReadGuard<'a, Environment>;
    pub type EnvMutRef<'a> = sync::RwLockWriteGuard<'a, Environment>;

    impl From<Environment> for EnvRef {
        fn from(env: Environment) -> Self {
            EnvRef::new(Ref::new(RwLock::new(env)))
        }
    }

    impl EnvRef {
        fn new(r: Ref<RwLock<Environment>>) -> Self {
            let mut e = EnvRef {
                ptr: 0 as *const _, //&*r.borrow(),
                env: r,
            };
            // I fervently hope that the content is never moved once placed in a Ref (Rc/Arc)
            e.ptr = {
                let x = &*e.borrow();
                x as *const _
            };
            e
        }

        pub fn borrow(&self) -> EnvTmpRef {
            self.env
                .read()
                .expect("unable to lock environment for reading")
        }

        pub fn borrow_mut(&self) -> EnvMutRef {
            self.env
                .write()
                .expect("unable to lock environment for writing")
        }

        pub fn downgrade(&self) -> EnvWeak {
            EnvWeak(Ref::downgrade(&self.env))
        }

        pub fn as_ptr(&self) -> *const Environment {
            self.ptr
        }
    }

    #[derive(Clone, Default)]
    pub struct EnvWeak(Weak<RwLock<Environment>>);

    impl EnvWeak {
        pub fn upgrade(&self) -> Option<EnvRef> {
            self.0.upgrade().map(EnvRef::new)
        }
    }
}

#[cfg(not(feature = "thread-safe"))]
mod env_ref_impl {
    use crate::environment::Environment;
    use crate::expression::{Ref, Weak};
    use std::cell::{self, RefCell};

    #[derive(Clone)]
    pub struct EnvRef(Ref<RefCell<Environment>>);

    pub type EnvTmpRef<'a> = cell::Ref<'a, Environment>;
    pub type EnvMutRef<'a> = cell::RefMut<'a, Environment>;

    impl From<Environment> for EnvRef {
        fn from(env: Environment) -> Self {
            EnvRef(Ref::new(RefCell::new(env)))
        }
    }

    impl EnvRef {
        pub fn borrow(&self) -> EnvTmpRef {
            self.0.borrow()
        }

        pub fn borrow_mut(&self) -> EnvMutRef {
            self.0.borrow_mut()
        }

        pub fn downgrade(&self) -> EnvWeak {
            EnvWeak(Ref::downgrade(&self.0))
        }

        pub fn as_ptr(&self) -> *const Environment {
            self.0.as_ptr()
        }
    }

    #[derive(Clone, Default)]
    pub struct EnvWeak(Weak<RefCell<Environment>>);

    impl EnvWeak {
        pub fn upgrade(&self) -> Option<EnvRef> {
            self.0.upgrade().map(EnvRef)
        }
    }
}
