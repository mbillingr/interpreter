use super::run;
use crate::Expression;
use crate::environment::default_env;
use crate::eval;
use crate::symbol::Symbol;

#[test]
fn eqv() {
    assert_eq!(Expression::True, run("(eqv? #t #t)").unwrap());
    assert_eq!(Expression::True, run("(eqv? #f #f)").unwrap());
    assert_eq!(Expression::True, run("(eqv? 'a 'a)").unwrap());
    assert_eq!(Expression::True, run("(eqv? 3 3)").unwrap());
    assert_eq!(Expression::True, eval(&Expression::from_vec(vec![Expression::Symbol(Symbol::new("eqv?")), Expression::Char('x'), Expression::Char('x')]), default_env()).unwrap());
    assert_eq!(Expression::True, run("(eqv? '() '())").unwrap());
    assert_eq!(Expression::True, run("(define x (cons 1 2)) (eqv? x x)").unwrap());
    assert_eq!(Expression::True, run("(define x \"xyz\") (eqv? x x)").unwrap());
    assert_eq!(Expression::True, run("(eqv? + +)").unwrap());
    assert_eq!(Expression::True, run("
        (define (a x) (* x x))
        (define b a)
        (eqv? a b)"
    ).unwrap());

    assert_eq!(Expression::False, run("(eqv? '(2) 2)").unwrap());
    assert_eq!(Expression::False, run("(eqv? '(2) \"2\")").unwrap());
    assert_eq!(Expression::False, run("(eqv? #f #t)").unwrap());
    assert_eq!(Expression::False, run("(eqv? #t #f)").unwrap());
    assert_eq!(Expression::False, run("(eqv? 'a 'b)").unwrap());
    assert_eq!(Expression::False, run("(eqv? 2 2.0)").unwrap());
    assert_eq!(Expression::False, run("(eqv? 3 6)").unwrap());
    assert_eq!(Expression::False, run("(eqv? 3.001 3.002)").unwrap());
    assert_eq!(Expression::False, eval(&Expression::from_vec(vec![Expression::Symbol(Symbol::new("eqv?")), Expression::Char('x'), Expression::Char('y')]), default_env()).unwrap());
    assert_eq!(Expression::False, run("(eqv? '() '(2))").unwrap());
    assert_eq!(Expression::False, run("(eqv? '(1) '())").unwrap());
    assert_eq!(Expression::False, run("(eqv? (cons 1 2) (cons 1 2))").unwrap());
    assert_eq!(Expression::False, run("(eqv? \"abc\" \"xyz\")").unwrap());
    assert_eq!(Expression::False, run("(eqv? + -)").unwrap());
    assert_eq!(Expression::False, run("
        (define (a x) (* x x))
        (define (b x) (+ x x))
        (eqv? a b)"
    ).unwrap());
}

#[test]
fn eq() {
    assert_eq!(Expression::True, run("(eq? 'a 'a)").unwrap());
    assert_eq!(Expression::False, run("(eq? (list 'a) (list 'a))").unwrap());
    assert_eq!(Expression::True, run("(eq? '() '())").unwrap());
    assert_eq!(Expression::True, run("(eq? car car)").unwrap());
    assert_eq!(Expression::True, run("(let ((x '(a))) (eq? x x))").unwrap());
}
#[test]
fn equal() {
    assert_eq!(Expression::True, run("(equal? 'a 'a)").unwrap());
    assert_eq!(Expression::True, run("(equal? '(a) '(a))").unwrap());
    assert_eq!(Expression::True, run("(equal? '(a (b) c) '(a (b) c))").unwrap());
    assert_eq!(Expression::True, run("(equal? \"abc\" \"abc\")").unwrap());
    assert_eq!(Expression::True, run("(equal? 2 2)").unwrap());

    assert_eq!(Expression::True, run("(equal? (lambda (x) (* x x)) (lambda (x) (* x x)))").unwrap());
    //assert_eq!(Expression::True, run("(equal? (lambda (x) (* x x)) (lambda (y) (* y y)))").unwrap());
    assert_eq!(Expression::False, run("(equal? (lambda (x) (* x x)) (lambda (x) (+ x x)))").unwrap());

}