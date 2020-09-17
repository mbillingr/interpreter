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
