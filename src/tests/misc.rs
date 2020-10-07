use super::run_in_env;
use crate::environment::default_env;
use crate::Expression;

#[test]
fn normal_let() {
    let env = default_env();

    assert_eq!(
        Expression::int(12),
        run_in_env(
            "
(let ((n 10) (x 2))
  (+ n x))
",
            &env
        )
        .unwrap()
    );
}

#[test]
fn named_let() {
    let env = default_env();

    assert_eq!(
        Expression::int(2048),
        run_in_env(
            "
(let loop ((n 10) (x 2))
  (if (= n 0)
      x
      (loop (- n 1) (* x 2))))
",
            &env
        )
        .unwrap()
    );
}

#[test]
fn type_predicates() {
    let env = default_env();

    assert_eq!(run_in_env("(null? '())", &env).unwrap(), Expression::True,);

    assert_eq!(run_in_env("(null? 0)", &env).unwrap(), Expression::False,);

    assert_eq!(run_in_env("(boolean? 1)", &env).unwrap(), Expression::False,);

    assert_eq!(run_in_env("(boolean? #f)", &env).unwrap(), Expression::True,);

    assert_eq!(run_in_env("(boolean? #t)", &env).unwrap(), Expression::True,);

    assert_eq!(
        run_in_env("(char? \"x\")", &env).unwrap(),
        Expression::False,
    );

    assert_eq!(run_in_env("(char? #\\x)", &env).unwrap(), Expression::True,);

    assert_eq!(
        run_in_env("(complex? \"0\")", &env).unwrap(),
        Expression::False,
    );

    assert_eq!(run_in_env("(complex? 12)", &env).unwrap(), Expression::True,);

    assert_eq!(
        run_in_env("(complex? 1/2)", &env).unwrap(),
        Expression::True,
    );

    assert_eq!(
        run_in_env("(complex? 1.2)", &env).unwrap(),
        Expression::True,
    );

    assert_eq!(
        run_in_env("(complex? 1+2i)", &env).unwrap(),
        Expression::True,
    );

    assert_eq!(
        run_in_env("(integer? \"0\")", &env).unwrap(),
        Expression::False,
    );

    assert_eq!(run_in_env("(integer? 0)", &env).unwrap(), Expression::True,);

    assert_eq!(
        run_in_env("(integer? 1/2)", &env).unwrap(),
        Expression::False,
    );

    assert_eq!(
        run_in_env("(integer? 1.2)", &env).unwrap(),
        Expression::False,
    );

    assert_eq!(
        run_in_env("(integer? 1+2i)", &env).unwrap(),
        Expression::False,
    );

    assert_eq!(
        run_in_env("(number? \"0\")", &env).unwrap(),
        Expression::False,
    );

    assert_eq!(run_in_env("(number? 0)", &env).unwrap(), Expression::True,);

    assert_eq!(run_in_env("(number? 1/2)", &env).unwrap(), Expression::True,);

    assert_eq!(run_in_env("(number? 1.2)", &env).unwrap(), Expression::True,);

    assert_eq!(
        run_in_env("(number? 1+2i)", &env).unwrap(),
        Expression::True,
    );

    assert_eq!(run_in_env("(pair? '())", &env).unwrap(), Expression::False,);

    assert_eq!(
        run_in_env("(pair? '(1 . 2))", &env).unwrap(),
        Expression::True,
    );

    assert_eq!(run_in_env("(pair? '(x))", &env).unwrap(), Expression::True,);

    assert_eq!(
        run_in_env("(procedure? '())", &env).unwrap(),
        Expression::False,
    );

    assert_eq!(
        run_in_env("(procedure? procedure?)", &env).unwrap(),
        Expression::True,
    );

    assert_eq!(
        run_in_env("(real? \"0\")", &env).unwrap(),
        Expression::False,
    );

    assert_eq!(run_in_env("(real? 0)", &env).unwrap(), Expression::True,);

    assert_eq!(run_in_env("(real? 1/2)", &env).unwrap(), Expression::True,);

    assert_eq!(run_in_env("(real? 1.2)", &env).unwrap(), Expression::True,);

    assert_eq!(run_in_env("(real? 1+2i)", &env).unwrap(), Expression::False,);

    assert_eq!(
        run_in_env("(rational? \"0\")", &env).unwrap(),
        Expression::False,
    );

    assert_eq!(run_in_env("(rational? 0)", &env).unwrap(), Expression::True,);

    assert_eq!(
        run_in_env("(rational? 1/2)", &env).unwrap(),
        Expression::True,
    );

    assert_eq!(
        run_in_env("(rational? 1.2)", &env).unwrap(),
        Expression::False,
    );

    assert_eq!(
        run_in_env("(rational? 1+2i)", &env).unwrap(),
        Expression::False,
    );

    assert_eq!(
        run_in_env("(string? \"0\")", &env).unwrap(),
        Expression::True,
    );

    assert_eq!(run_in_env("(string? 0)", &env).unwrap(), Expression::False,);

    assert_eq!(
        run_in_env("(symbol? \"x\")", &env).unwrap(),
        Expression::False,
    );

    assert_eq!(run_in_env("(symbol? 'x)", &env).unwrap(), Expression::True,);

    assert_eq!(
        run_in_env("(vector? '(1 2 3))", &env).unwrap(),
        Expression::False,
    );

    assert_eq!(
        run_in_env("(vector? #(1 2 3))", &env).unwrap(),
        Expression::True,
    );
}
