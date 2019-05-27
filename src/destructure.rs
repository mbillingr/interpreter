macro_rules! destructure {
    ($x:expr => $car:ident) => {
        $x.car()
            .and_then(|a| convert!(a => $car))
            .map(|a| combine!(a,))
    };

    ($x:expr => $car:ident, $($cdr:ident),*) => {
        $x.car()
            .and_then(|a| convert!(a => $car))
            .and_then(|a| Ok(combine!(a, destructure!($x.cdr()? => $($cdr),*)?)))
    };
}

macro_rules! convert {
    ($x:expr => expr) => {{
        let r: $crate::errors::Result<_> = Ok($x);
        r
    }};

    ($x:expr => auto) => {
        $x.try_into()
    };
}

macro_rules! combine {
    ($a:expr, $b:expr) => {
        ($a, $b)
    };
    ($a:expr, ) => {
        ($a,)
    };
}

#[cfg(test)]
mod test {
    use crate::expression::Expression as X;
    use std::convert::TryInto;

    #[test]
    fn destructure() {
        let x = X::from_vec(vec![1.into(), 2.into(), 3.into()]);

        assert_eq!((&X::Integer(1),), destructure!(x => expr).unwrap());
        assert_eq!(
            (&X::Integer(1), (&X::Integer(2),)),
            destructure!(x => expr, expr).unwrap()
        );
        assert_eq!(
            (&X::Integer(1), (&X::Integer(2), (&X::Integer(3),))),
            destructure!(x => expr, expr, expr).unwrap()
        );

        assert_eq!((1_i64,), destructure!(x => auto).unwrap());
        assert_eq!((1_i64, (2_i64,)), destructure!(x => auto, auto).unwrap());

        let (a, (b, (c,))) = destructure!(x => auto, auto, auto).unwrap();
        assert_eq!(1_i64, a);
        assert_eq!(2_i64, b);
        assert_eq!(3_i64, c);

        let x = X::from_vec(vec![1.into(), "2".into(), true.into()]);

        let (a, (b, (c,))): (i64, (&str, (bool,))) = destructure!(x => auto, auto, auto).unwrap();
        assert_eq!(1_i64, a);
        assert_eq!("2", b);
        assert_eq!(true, c);
    }
}

/*use crate::errors::*;
use crate::expression::{Expression as X, List};

pub trait Destructure<T> {
    fn destructure(self) -> Result<T>
    where
        Self: Sized,
    {
        let (front, tail) = self.tail_destructure()?;
        if !tail.is_empty() {
            return Err(ErrorKind::ArgumentError)?;
        }
        Ok(front)
    }

    fn tail_destructure(self) -> Result<(T, List)>;
}

impl<T> Destructure<T> for X
where
    List: Destructure<T>,
{
    fn destructure(self) -> Result<T> {
        self.try_into_list()?.destructure()
    }

    fn tail_destructure(self) -> Result<(T, List)> {
        self.try_into_list()?.tail_destructure()
    }
}

impl Destructure<X> for List {
    fn tail_destructure(self) -> Result<(X, List)> {
        let mut list = self.into_iter();
        let front = list.next().ok_or(ErrorKind::ArgumentError)?;
        let tail = list.collect();
        Ok((front, tail))
    }
}

impl Destructure<(X,)> for List {
    fn tail_destructure(self) -> Result<((X,), List)> {
        let mut list = self.into_iter();
        let front = list.next().ok_or(ErrorKind::ArgumentError)?;
        let tail = list.collect();
        Ok(((front,), tail))
    }
}

impl Destructure<[X; 1]> for List {
    fn tail_destructure(self) -> Result<([X; 1], List)> {
        let mut list = self.into_iter();
        let front = list.next().ok_or(ErrorKind::ArgumentError)?;
        let tail = list.collect();
        Ok(([front], tail))
    }
}

impl Destructure<[X; 2]> for List {
    fn tail_destructure(self) -> Result<([X; 2], List)> {
        let mut list = self.into_iter();
        let front = [
            list.next().ok_or(ErrorKind::ArgumentError)?,
            list.next().ok_or(ErrorKind::ArgumentError)?,
        ];
        let tail = list.collect();
        Ok((front, tail))
    }
}

impl Destructure<[X; 3]> for List {
    fn tail_destructure(self) -> Result<([X; 3], List)> {
        let mut list = self.into_iter();
        let front = [
            list.next().ok_or(ErrorKind::ArgumentError)?,
            list.next().ok_or(ErrorKind::ArgumentError)?,
            list.next().ok_or(ErrorKind::ArgumentError)?,
        ];
        let tail = list.collect();
        Ok((front, tail))
    }
}

impl Destructure<[X; 4]> for List {
    fn tail_destructure(self) -> Result<([X; 4], List)> {
        let mut list = self.into_iter();
        let front = [
            list.next().ok_or(ErrorKind::ArgumentError)?,
            list.next().ok_or(ErrorKind::ArgumentError)?,
            list.next().ok_or(ErrorKind::ArgumentError)?,
            list.next().ok_or(ErrorKind::ArgumentError)?,
        ];
        let tail = list.collect();
        Ok((front, tail))
    }
}

impl Destructure<(X, X)> for List {
    fn tail_destructure(self) -> Result<((X, X), List)> {
        if self.len() < 2 {
            return Err(ErrorKind::ArgumentError)?;
        }
        let mut list = self.into_iter();
        let front = (list.next().unwrap(), list.next().unwrap());
        let tail = list.collect();
        Ok((front, tail))
    }
}

impl Destructure<(X, List)> for List {
    fn tail_destructure(self) -> Result<((X, List), List)> {
        let (front, tail): ((X, X), _) = self.tail_destructure()?;
        let front = (front.0, front.1.try_into_list()?);
        Ok((front, tail))
    }
}

impl Destructure<(List, X)> for List {
    fn tail_destructure(self) -> Result<((List, X), List)> {
        let (front, tail): ((X, X), _) = self.tail_destructure()?;
        let front = (front.0.try_into_list()?, front.1);
        Ok((front, tail))
    }
}

impl Destructure<(List, List)> for List {
    fn tail_destructure(self) -> Result<((List, List), List)> {
        let (front, tail): ((X, X), _) = self.tail_destructure()?;
        let front = (front.0.try_into_list()?, front.1.try_into_list()?);
        Ok((front, tail))
    }
}

impl Destructure<(X, X, X)> for List {
    fn tail_destructure(self) -> Result<((X, X, X), List)> {
        if self.len() < 3 {
            return Err(ErrorKind::ArgumentError)?;
        }
        let mut list = self.into_iter();
        let front = (
            list.next().unwrap(),
            list.next().unwrap(),
            list.next().unwrap(),
        );
        let tail = list.collect();
        Ok((front, tail))
    }
}

impl Destructure<(X, X, List)> for List {
    fn tail_destructure(self) -> Result<((X, X, List), List)> {
        let (front, tail): ((X, X, X), _) = self.tail_destructure()?;
        let front = (front.0, front.1, front.2.try_into_list()?);
        Ok((front, tail))
    }
}

impl Destructure<(X, List, X)> for List {
    fn tail_destructure(self) -> Result<((X, List, X), List)> {
        let (front, tail): ((X, X, X), _) = self.tail_destructure()?;
        let front = (front.0, front.1.try_into_list()?, front.2);
        Ok((front, tail))
    }
}

impl Destructure<(X, List, List)> for List {
    fn tail_destructure(self) -> Result<((X, List, List), List)> {
        let (front, tail): ((X, X, X), _) = self.tail_destructure()?;
        let front = (front.0, front.1.try_into_list()?, front.2.try_into_list()?);
        Ok((front, tail))
    }
}

impl Destructure<(List, X, X)> for List {
    fn tail_destructure(self) -> Result<((List, X, X), List)> {
        let (front, tail): ((X, X, X), _) = self.tail_destructure()?;
        let front = (front.0.try_into_list()?, front.1, front.2);
        Ok((front, tail))
    }
}

impl Destructure<(List, X, List)> for List {
    fn tail_destructure(self) -> Result<((List, X, List), List)> {
        let (front, tail): ((X, X, X), _) = self.tail_destructure()?;
        let front = (front.0.try_into_list()?, front.1, front.2.try_into_list()?);
        Ok((front, tail))
    }
}

impl Destructure<(List, List, X)> for List {
    fn tail_destructure(self) -> Result<((List, List, X), List)> {
        let (front, tail): ((X, X, X), _) = self.tail_destructure()?;
        let front = (front.0.try_into_list()?, front.1.try_into_list()?, front.2);
        Ok((front, tail))
    }
}

impl Destructure<(List, List, List)> for List {
    fn tail_destructure(self) -> Result<((List, List, List), List)> {
        let (front, tail): ((X, X, X), _) = self.tail_destructure()?;
        let front = (
            front.0.try_into_list()?,
            front.1.try_into_list()?,
            front.2.try_into_list()?,
        );
        Ok((front, tail))
    }
}
*/
