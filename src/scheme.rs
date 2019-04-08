/// construct Scheme expressions from Rust code.
///
/// Examples:
///     `scheme!(42)`  =>  42
///     `scheme!((42))`  =>  (42)
///     `scheme!(42, "abc")`  => (42 "abc")
///     `scheme!((42, "abc"))`  => (42 "abc")
///     `scheme!(abc)`  => (abc)
///     `let x = 5; scheme!(x, @x)`  => (x 5)
///
// much inspired by serde_json's json! macro
// todo: which is better?
//       a) pass in local variables as they are and symbols as strings: `scheme!("lambda", vars, body)`
//       b) symbols as identifiers and local variables as - what? ... e.g.: `scheme!(lambda, @vars, @body)`
//    current favorite (b) because how would we pass string literals in (a)?
macro_rules! scheme {
    () => { $crate::Expression::Nil };
    (()) => { $crate::Expression::Nil };
    (#t) => { $crate::Expression::True };
    (#f) => { $crate::Expression::False };

    // pair
    ($car:tt . $cdr:tt) => {
        $crate::Expression::cons(scheme!($car), scheme!($cdr))
    };

    // pair
    (($car:tt . $cdr:tt)) => {
        $crate::Expression::cons(scheme!($car), scheme!($cdr))
    };

    ( ($($tt:tt)+) ) => {
        scheme!(@list () $($tt)+)
    };

    // first item starts with @
    ( @ $first:tt, $($rest:tt)* ) => {
        scheme!(@list (scheme!(@$first)), $($rest)* )
    };

    ( $first:tt, $($rest:tt)* ) => {
        scheme!(@list (scheme!($first)), $($rest)* )
    };

    // Done with trailing comma.
    (@list ($($elems:expr,)*)) => {
        conslist![$($elems,)*]
    };

    // Done without trailing comma.
    (@list ($($elems:expr),*)) => {
        conslist![$($elems),*]
    };

    // Next element is `true`.
    (@list ($($elems:expr,)*) #t $($tail:tt)*) => {
        scheme!(@list ($($elems,)* scheme!(#t)) $($tail)*)
    };

    // Next element is `false`.
    (@list ($($elems:expr,)*) #f $($tail:tt)*) => {
        scheme!(@list ($($elems,)* scheme!(#f)) $($tail)*)
    };

    // Next element is a list.
    (@list ($($elems:expr,)*) ($($list:tt)*) $($rest:tt)*) => {
        scheme!(@list ($($elems,)* scheme!(($($list)*))) $($rest)*)
    };

    // Last element is a list expression to append.
    (@list ($($elems:expr,)*) ...$last:expr) => {
        conslist![$($elems),* , ($last)]
    };

    // Next element is an identifier followed by comma.
    (@list ($($elems:expr,)*) $next:ident, $($tail:tt)*) => {
        scheme!(@list ($($elems,)* scheme!($next),) $($tail)*)
    };

    // Last element is an identifier with no trailing comma.
    (@list ($($elems:expr,)*) $last:ident) => {
        scheme!(@list ($($elems,)* scheme!($last)))
    };

    // Next element is an explicit expression followed by comma.
    (@list ($($elems:expr,)*) @$next:expr, $($tail:tt)*) => {
        scheme!(@list ($($elems,)* scheme!(@$next),) $($tail)*)
    };

    // Last element is an explicit expression with no trailing comma.
    (@list ($($elems:expr,)*) @$last:expr) => {
        scheme!(@list ($($elems,)* scheme!(@$last)))
    };

    // Next element is an expression followed by comma.
    (@list ($($elems:expr,)*) $next:expr, $($tail:tt)*) => {
        scheme!(@list ($($elems,)* scheme!($next),) $($tail)*)
    };

    // Last element is an expression with no trailing comma.
    (@list ($($elems:expr,)*) $last:expr) => {
        scheme!(@list ($($elems,)* scheme!($last)))
    };

    // Comma after the most recent element.
    (@list ($($elems:expr),*) , $($tail:tt)*) => {
        scheme!(@list ($($elems,)*) $($tail)*)
    };

    ($symbol:ident) => {{
        $crate::Expression::Symbol(stringify!($symbol).into())
    }};

    (@$other:expr) => {{
        let expr: $crate::Expression = $other.into();
        expr
    }};

    ($other:expr) => {{
        let expr: $crate::Expression = $other.into();
        expr
    }};
}

macro_rules! conslist {
    () => { $crate::Expression::Nil };

    // no trailing comma, one item left
    ($car:expr) => {
        $crate::Expression::cons($car, $crate::Expression::Nil)
    };

    // parse with trailing comma
    ($car:expr, ($cdr:expr)) => {
        $crate::Expression::cons($car, $cdr)
    };

    // parse with trailing comma
    ($car:expr, $($cdr:tt)*) => {
        $crate::Expression::cons($car, conslist!($($cdr)*))
    };
}

#[cfg(test)]
mod test {
    use crate::expression::Expression as X;

    fn v2c(x: Vec<impl Into<X>>) -> X {
        let mut cdr = X::Nil;
        for car in x.into_iter().rev() {
            cdr = X::cons(car, cdr);
        }
        cdr
    }

    #[test]
    #[rustfmt::skip]
    fn scheme_macro() {
        assert_eq!(v2c(vec![1, 2, 3]), conslist![1, 2, 3]);
        assert_eq!(v2c(vec![1, 2, 3]), conslist![1, 2, 3,]);

        assert_eq!(scheme!(), X::Nil);
        assert_eq!(X::True, scheme!(#t));
        assert_eq!(X::False, scheme!(#f));
        assert_eq!(X::Integer(1), scheme!(1));
        assert_eq!(X::from("abc"), scheme!("abc"));
        assert_eq!(X::from_literal("xyz"), scheme!(xyz));
        assert_eq!(conslist![X::from_literal("cond")], scheme!(cond,));
        assert_eq!(
            X::cons(
                X::from_literal("lambda"),
                X::cons(
                    X::cons(X::from_literal("a"), X::cons(X::from_literal("b"), X::Nil)),
                    X::cons(
                        X::cons(
                            X::from_literal("gcd"),
                            X::cons(X::from_literal("a"), X::cons(X::from_literal("b"), X::Nil))
                        ),
                        X::Nil
                    )
                )
            ),
            scheme!(lambda, (a, b), (gcd, a, b))
        );
        assert_eq!(v2c(vec![1, 2, 3]), scheme!(1, 2, 3));
        assert_eq!(v2c(vec![1, 2, 3]), scheme!(1, 2, 3,));
        assert_eq!(
            X::cons(1, X::cons(v2c(vec![2, 3]), X::cons(4, X::Nil))),
            scheme!(1, (2, 3), 4)
        );
        assert_eq!(
            X::cons(
                1,
                X::cons(v2c(vec![2, 3, 4]), X::cons(v2c(vec![5, 6]), X::Nil))
            ),
            scheme!(1, (2, 3, 4), (5, 6,))
        );
        assert_eq!(X::cons(1, 2), scheme!(1 . 2));
        assert_eq!(v2c(vec![X::cons(1, 2)]), scheme!((1 . 2),));
        assert_eq!(
            v2c(vec![X::cons(1, 2), X::cons(2, 3)]),
            scheme!((1 . 2), (2 . 3))
        );
        assert_eq!(X::cons(1, X::cons(2, 3)), scheme!((1 . (2 . 3))));
        assert_eq!(
            X::cons(1, X::cons(2, X::cons(3, 4))),
            scheme!((1 . (2 . (3 . 4))))
        );
        assert_eq!(v2c(vec![1, 2, 3]), scheme!((1 . (2 . (3 . ())))));

        let a = 42;
        let b = scheme!(4, 2);

        assert_eq!(
            X::cons(42, X::cons(v2c(vec![4, 2]), X::Nil)),
            scheme!(@a, @b.clone())
        );
        assert_eq!(
            X::cons(
                X::cons(42, X::cons(42, X::Nil)),
                X::cons(v2c(vec![4, 2]), X::Nil)
            ),
            scheme!((@a, @a), @b.clone())
        );
        assert_eq!(v2c(vec![42, 42, 4, 2]), scheme!(@a, @a, ...b));
    }
}
