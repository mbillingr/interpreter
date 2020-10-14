use std::ops::Deref;

#[derive(Copy, PartialEq)]
pub struct Ref<T: 'static> {
    inner: &'static T,
}

impl<T: 'static> Ref<T> {
    pub fn new(obj: T) -> Self {
        Ref {
            inner: Box::leak(Box::new(obj)),
        }
    }

    pub fn downgrade(this: &Ref<T>) -> Weak<T> {
        Weak {
            maybe_inner: Some(this.inner),
        }
    }

    pub fn get_mut(this: &mut Ref<T>) -> Option<&mut T> {
        let ptr = this.inner as *const T;
        let mutptr = ptr as *mut T;
        let mutref = unsafe { &mut *mutptr };
        Some(mutref)
    }

    pub fn ptr_eq(this: &Ref<T>, other: &Ref<T>) -> bool {
        std::ptr::eq(this.inner, other.inner)
    }
}

impl<T: Clone + 'static> Ref<T> {
    pub fn try_unwrap(this: Ref<T>) -> Result<T, Ref<T>> {
        Ok(this.inner.clone())
    }
}

impl<T: 'static> Clone for Ref<T> {
    fn clone(&self) -> Self {
        Ref { inner: self.inner }
    }
}

impl<T: 'static> Deref for Ref<T> {
    type Target = T;
    #[inline(always)]
    fn deref(&self) -> &T {
        self.inner
    }
}

impl<T: 'static> AsRef<T> for Ref<T> {
    fn as_ref(&self) -> &T {
        &**self
    }
}

impl<T: std::fmt::Debug + 'static> std::fmt::Debug for Ref<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(&**self, f)
    }
}

impl<T: std::fmt::Display + 'static> std::fmt::Display for Ref<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(&**self, f)
    }
}

#[derive(Copy)]
pub struct Weak<T: 'static> {
    maybe_inner: Option<&'static T>,
}

impl<T: 'static> Weak<T> {
    pub fn upgrade(&self) -> Option<Ref<T>> {
        self.maybe_inner.map(|inner| Ref { inner })
    }
}

impl<T: 'static> Default for Weak<T> {
    fn default() -> Self {
        Weak { maybe_inner: None }
    }
}

impl<T: 'static> Clone for Weak<T> {
    fn clone(&self) -> Self {
        Weak {
            maybe_inner: self.maybe_inner,
        }
    }
}
