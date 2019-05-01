(define-library (sicp generic scheme-number)

  (export make-scheme-number)

  (import (builtin core)
          (sicp utils)
          (sicp generic))

  (begin
    (define (make-scheme-number n)
      ((get 'make 'scheme-number) n))

    (define (reduce-integers n d)
      (let ((g (gcd n d)))
        (list (/ n g) (/ d g))))

    (define (tag x) (attach-tag 'scheme-number x))
    (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))
    (put 'neg '(scheme-number) (lambda (x) (tag (- x))))
    (put 'pow '(scheme-number scheme-number) (lambda (x y) (tag (power x y))))
    (put 'equ? '(scheme-number scheme-number) =)
    (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
    (put 'greatest-common-divisor '(scheme-number scheme-number)
      (lambda (x y) (tag (gcd x y))))
    (put 'reduce '(scheme-number scheme-number) reduce-integers)  ; I'm cheating here, leaving out tagging the two results which is supposed to be a no-op anyway
    (put 'print '(scheme-number) (lambda (x) (display x)))
    (put 'make 'scheme-number (lambda (x) (tag x)))))
