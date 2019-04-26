
(define-library (sicp generic math)
  (export =zero? add div equ? mul neg sub)

  (import (sicp generic))

  (begin
    (define (add x y) (apply-generic 'add x y))
    (define (sub x y) (apply-generic 'sub x y))
    (define (mul x y) (apply-generic 'mul x y))
    (define (div x y) (apply-generic 'div x y))
    (define (neg x) (apply-generic 'neg x))

    (define (equ? a b) (apply-generic 'equ? a b))
    (define (=zero? x) (apply-generic '=zero? x))))