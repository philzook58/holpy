//! This module implements a variant of `Rc` where

use std::{fmt, hash::Hash, ops::Deref, rc};

pub struct Rc<T: ?Sized>(rc::Rc<T>);

impl<T: ?Sized> Clone for Rc<T> {
    fn clone(&self) -> Self {
        // Self(self.0.clone());
        Self(rc::Rc::clone(&self.0))
    }
}

impl<T: ?Sized> PartialEq for Rc<T> {
    fn eq(&self, other: &Self) -> bool {
        rc::Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<T: ?Sized> Hash for Rc<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        rc::Rc::as_ptr(&self.0).hash(state)
    }
}

impl<T: ?Sized> Deref for Rc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

impl<T: ?Sized> AsRef<T> for Rc<T> {
    fn as_ref(&self) -> &T {
        self.0.as_ref()
    }
}

impl<T: ?Sized, U> From<U> for Rc<T>
where
    rc::Rc<T>: From<U>,
{
    fn from(inner: U) -> Self {
        Self(inner.into())
    }
}

// todo
impl<T, const N: usize> Rc<[T; N]> {
    /// Converts an `Rc` of an array into an `Rc` of a slice.
    pub fn to_rc_of_slice(self) -> Rc<[T]> {
        Rc(self.0 as _)
    }
}

impl<T: ?Sized + fmt::Debug> fmt::Debug for Rc<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

impl<T: ?Sized + fmt::Display> fmt::Display for Rc<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

impl<T> Rc<T> {
    /// Constructs a new `Rc<T>`.
    pub fn new(value: T) -> Self {
        #[allow(clippy::disallowed_methods)]
        Self(rc::Rc::new(value))
    }

    /// Similar to [`std::rc::Rc::strong_count`].
    pub fn strong_count(this: &Self) -> usize {
        rc::Rc::strong_count(&this.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rc() {
        let a = Rc::new(1);
        let b = Rc::new(1);
        println!("{:#?}", &a);
        println!("{:#?}", &b);
        assert_ne!(a, b);

        let c = Rc::clone(&b);
        let d = b.clone();
        assert_eq!(b, c);
        assert_eq!(b, d);
    }
}
