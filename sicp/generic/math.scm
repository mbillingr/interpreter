
(define-library (sicp generic math)
  (export =zero? add div equ? greatest-common-divisor mul neg sub pow)

  (import (sicp generic))

  (begin
    (define (add x y) (apply-generic 'add x y))
    (define (sub x y) (apply-generic 'sub x y))
    (define (mul x y) (apply-generic 'mul x y))
    (define (div x y) (apply-generic 'div x y))
    (define (neg x) (apply-generic 'neg x))
    (define (pow b e) (apply-generic 'pow b e))

    (define (equ? a b) (apply-generic 'equ? a b))
    (define (=zero? x) (apply-generic '=zero? x))

    (define (greatest-common-divisor a b)
      (apply-generic 'greatest-common-divisor a b))))
