// much inspired by serde_json's json! macro
// todo: which is better?
//       a) pass in local variables as they are and symbols as strings: `scheme!("lambda", vars, body)`
//       b) symbols as identifiers and local variables as - what? ... e.g.: `scheme!(lambda, @vars, @body)`
macro_rules! scheme {
    () => { Expression::Nil };
    (#t) => { Expression::True };
    (#f) => { Expression::False };

    ( ($($tt:tt)+) ) => {
        scheme!(@list () $($tt)+)
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

    // Last element is an identifier with no trailing comma.
/*    (@list ($($elems:expr,)*) $last:ident) => {
        scheme!(@list ($($elems,)* scheme!($last)))
    };*/

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

    /*($symbol:ident) => {{
        Expression::Symbol(stringify!($symbol).into())
    }};*/

    ($other:expr) => {{
        let expr: Expression = $other.into();
        expr
    }};
}
