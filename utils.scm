
(define nil (list))

(define (timeit f)
  (define (measure f start)
    (f)
    (- (runtime) start))
  (define (report n sum sqsum)
    (display n)
    (display "x -- ")
    (display (/ sum n))
    (display " +- ")
    (display (sqrt (/ (- sqsum (/ (sqr sum) n) (- n 1)))))
    (newline))
  (define (iter n sum sqsum end)
    (if (and (> n 3) (> (runtime) end))
        (report n sum sqsum)
        (let ((time (measure f (runtime))))
          (iter (+ n 1)
                (+ sum time)
                (+ sqsum (sqr time))
                end))))
  (iter 0 0 0 (+ (runtime) 1e5)))

(define (caar p) (car (car p)))
(define (cadr p) (car (cdr p)))
(define (cdar p) (cdr (car p)))
(define (cddr p) (cdr (cdr p)))

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


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


(define (average a b) (/ (+ a b) 2))
(define (sqr x) (* x x))
(define (cube x) (* x x x))
(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  (fixed-point improve 1.0))


(define (xor a b)
  (and (or a b)
       (not (and a b))))


(define tolerance 1e-12)

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (let ((next (improve guess)))
         (if (good-enough? guess next)
             next
             (iter next))))
  iter)

(define (fixed-point f first-guess)
  (define (good-enough? guess next)
    (< (abs (- guess next)) tolerance))
  ((iterative-improve good-enough? f) first-guess))


(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append (cdr list1)
                    list2))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map1 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(define (map op . seqs)
  (if (null? (car seqs))
      nil
      (cons (apply op (map1 car seqs))
            (apply map op (map1 cdr seqs)))))

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
