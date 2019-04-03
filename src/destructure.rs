use crate::errors::*;
use crate::expression::{Expression as X, List};

pub trait Destructure<T> {
    fn destructure(self) -> Result<T>;
}

impl<T> Destructure<T> for X
where
    List: Destructure<T>,
{
    fn destructure(self) -> Result<T> {
        self.try_into_list()?.destructure()
    }
}

impl Destructure<(X,)> for List {
    fn destructure(self) -> Result<(X,)> {
        if self.len() != 1 {
            return Err(ErrorKind::ArgumentError.into());
        }
        Ok((self.into_iter().next().unwrap(),))
    }
}

impl Destructure<(X, X)> for List {
    fn destructure(self) -> Result<(X, X)> {
        if self.len() != 2 {
            return Err(ErrorKind::ArgumentError.into());
        }
        let mut list = self.into_iter();
        Ok((list.next().unwrap(), list.next().unwrap()))
    }
}

impl Destructure<(X, List)> for List {
    fn destructure(self) -> Result<(X, List)> {
        let tmp: (X, X) = self.destructure()?;
        Ok((tmp.0, tmp.1.try_into_list()?))
    }
}

impl Destructure<(List, X)> for List {
    fn destructure(self) -> Result<(List, X)> {
        let tmp: (X, X) = self.destructure()?;
        Ok((tmp.0.try_into_list()?, tmp.1))
    }
}

impl Destructure<(List, List)> for List {
    fn destructure(self) -> Result<(List, List)> {
        let tmp: (X, X) = self.destructure()?;
        Ok((tmp.0.try_into_list()?, tmp.1.try_into_list()?))
    }
}

impl Destructure<(X, X, X)> for List {
    fn destructure(self) -> Result<(X, X, X)> {
        if self.len() != 3 {
            return Err(ErrorKind::ArgumentError.into());
        }
        let mut list = self.into_iter();
        Ok((
            list.next().unwrap(),
            list.next().unwrap(),
            list.next().unwrap(),
        ))
    }
}

impl Destructure<(X, X, List)> for List {
    fn destructure(self) -> Result<(X, X, List)> {
        let tmp: (X, X, X) = self.destructure()?;
        Ok((tmp.0, tmp.1, tmp.2.try_into_list()?))
    }
}

impl Destructure<(X, List, X)> for List {
    fn destructure(self) -> Result<(X, List, X)> {
        let tmp: (X, X, X) = self.destructure()?;
        Ok((tmp.0, tmp.1.try_into_list()?, tmp.2))
    }
}

impl Destructure<(X, List, List)> for List {
    fn destructure(self) -> Result<(X, List, List)> {
        let tmp: (X, X, X) = self.destructure()?;
        Ok((tmp.0, tmp.1.try_into_list()?, tmp.2.try_into_list()?))
    }
}

impl Destructure<(List, X, X)> for List {
    fn destructure(self) -> Result<(List, X, X)> {
        let tmp: (X, X, X) = self.destructure()?;
        Ok((tmp.0.try_into_list()?, tmp.1, tmp.2))
    }
}

impl Destructure<(List, X, List)> for List {
    fn destructure(self) -> Result<(List, X, List)> {
        let tmp: (X, X, X) = self.destructure()?;
        Ok((tmp.0.try_into_list()?, tmp.1, tmp.2.try_into_list()?))
    }
}

impl Destructure<(List, List, X)> for List {
    fn destructure(self) -> Result<(List, List, X)> {
        let tmp: (X, X, X) = self.destructure()?;
        Ok((tmp.0.try_into_list()?, tmp.1.try_into_list()?, tmp.2))
    }
}

impl Destructure<(List, List, List)> for List {
    fn destructure(self) -> Result<(List, List, List)> {
        let tmp: (X, X, X) = self.destructure()?;
        Ok((
            tmp.0.try_into_list()?,
            tmp.1.try_into_list()?,
            tmp.2.try_into_list()?,
        ))
    }
}
