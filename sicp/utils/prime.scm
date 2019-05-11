(define-library (sicp utils prime)
  (export fast-prime? prime?)

  (import (builtin core)
          (sicp utils))

  (begin
    (define (prime? n) (= n (smallest-divisor n)))

    (define (smallest-divisor n) (find-divisor n 2))

    (define (find-divisor n test-divisor)
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (next test-divisor)))))

    (define (square x) (* x x))

    (define (divides? a b) (= (remainder b a) 0))

    (define (next n)
      (if (= n 2) 3 (+ n 2)))

    (define (fast-prime? n times)
      (cond ((= times 0) true)
            ((miller-rabin-test n) (fast-prime? n (- times 1)))
            (else false)))

    (define (miller-rabin-test n)
      (define (try-it a)
          (= (expmod a (- n 1) n) 1))
      (try-it (+ 1 (random (- n 1)))))

    (define (expmod base exp m)
      (cond ((= exp 0) 1)
            ((even? exp) (remainder
                           (square (expmod base (/ exp 2) m))
                           m))
            (else (remainder
                    (* base (expmod base (- exp 1) m))
                    m))))))
