use super::run_in_env;
use crate::environment::default_env;
use crate::interpreter::Return;
use crate::Expression;

#[test]
fn apply() {
    let env = default_env();
    env.borrow_mut().insert_native("sqrt", |args| {
        args.car()
            .and_then(|x| x.try_as_float())
            .map(f64::sqrt)
            .map(Expression::from)
            .map(Return::Value)
    });

    assert_eq!(
        Expression::int(7),
        run_in_env("(apply +  (list 3 4))", &env).unwrap()
    );

    run_in_env(
        "(define compose (lambda (f g) (lambda args (f (apply g args)))))",
        &env,
    )
    .unwrap();
    assert_eq!(
        Expression::number(30.0),
        run_in_env("((compose sqrt *) 12 75)", &env).unwrap()
    );
}
