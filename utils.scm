(define (timed x) (measure x (runtime)))

(define (measure x start)
 (report (f-recursive x) (- (runtime) start)))

(define (report result elapsed)
 (display result)
 (display " -- ")
 (display elapsed))

(define (abs x) (if (< x 0) (- x) x))

(define (even? x) (= 0 (modulo x 2)))

(define (modulo a b)
 (define (fix a b r)
  (cond ((and (< a 0) (> b 0)) (+ b r))
        ((and (> a 0) (< b 0)) (+ b r))
        (else r)))
 (fix a b (remainder a b))
)

(define (sqr x) (* x x))
(define (cube x) (* x x x))
