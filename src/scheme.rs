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
    () => { Expression::Nil };
    (#t) => { Expression::True };
    (#f) => { Expression::False };

    ( ($($tt:tt)+) ) => {
        scheme!(@list () $($tt)+)
    };

    ( $first:tt, $($rest:tt)* ) => {
        scheme!(@list (scheme!($first)), $($rest)+ )
    };

    // Done with trailing comma.
    (@list ($($elems:expr,)*)) => {
        Expression::List(vec![$($elems,)*])
    };

    // Done without trailing comma.
    (@list ($($elems:expr),*)) => {
        Expression::List(vec![$($elems),*])
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

    // Unexpected token after most recent element.
    (@array [$($elems:expr),*] $unexpected:tt $($rest:tt)*) => {
        json_unexpected!($unexpected)
    };

    ($symbol:ident) => {{
        Expression::Symbol(stringify!($symbol).into())
    }};

    (@$other:expr) => {{
        let expr: Expression = $other.into();
        expr
    }};

    ($other:expr) => {{
        let expr: Expression = $other.into();
        expr
    }};
}
