(define (timed x) (measure x (runtime)))

(define (measure x start)
(report (f-recursive x) (- (runtime) start)))

(define (report result elapsed)
(display result)
(display " -- ")
(display elapsed))

(define (abs x) (if (< x 0) (- x) x))
(define (sqr x) (* x x))
(define (cube x) (* x x x))
