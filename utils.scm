(define (timed x) (measure x (runtime)))

(define (measure x start)
 (report (f-recursive x) (- (runtime) start)))

(define (report result elapsed)
 (display result)
 (display " -- ")
 (display elapsed))

(define (abs x) (if (< x 0) (- x) x))
(define (inc n) (+ n 1))
(define (dec n) (- n 1))

(define (even? x) (= 0 (modulo x 2)))

(define (modulo a b)
 (define (fix a b r)
  (cond ((and (< a 0) (> b 0)) (+ b r))
        ((and (> a 0) (< b 0)) (+ b r))
        (else r)))
 (fix a b (remainder a b)))


(define (sqr x) (* x x))
(define (cube x) (* x x x))


;; ==========================================
;;   useless stuff
;; ==========================================


(define (count-iter n)
  (define (dec n) (- n 1))
  (if (= n 0)
      0
      (count-iter (dec n))))

(define (make-sqr)
  (define (s x) (* x x))
  s)
  
