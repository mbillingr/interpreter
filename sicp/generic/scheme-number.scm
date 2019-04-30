(define-library (sicp generic scheme-number)

  (export make-scheme-number)

  (import (builtin core)
          (sicp utils)
          (sicp generic))

  (begin
    (define (tag x) (attach-tag 'scheme-number x))
    (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))
    (put 'neg '(scheme-number) (lambda (x) (tag (- x))))
    (put 'pow '(scheme-number scheme-number) (lambda (x y) (tag (exp x y))))
    (put 'equ? '(scheme-number scheme-number) =)
    (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
    (put 'greatest-common-divisor '(scheme-number scheme-number)
      (lambda (x y) (tag (gcd x y))))
    (put 'print '(scheme-number) (lambda (x) (display x)))
    (put 'make 'scheme-number (lambda (x) (tag x)))

    (define (make-scheme-number n)
      ((get 'make 'scheme-number) n))

    (define (exp base exponent)
      (define (exp-iter a b n)
        (cond ((= n 0) a)
              ((even? n) (exp-iter a
                                   (* b b)
                                   (/ n 2)))
              (else (exp-iter (* a b)
                              b
                              (- n 1)))))
      (exp-iter 1 base exponent))))
