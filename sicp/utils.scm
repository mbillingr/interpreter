
(define-library (sicp utils)

  (export <= >=
          abs accumulate append average
          caar cadr cdar cddr caaar caddr cdadr cddar cdddr cadddr cube
          debug-print dec
          even?
          false fixed-point
          gcd get get-coercion
          inc iterative-improve
          length
          map map1 memq modulo
          nil
          power println put put-coercion
          timeit true
          sqr sqrt symbol<?
          xor)

  (import (builtin core)
          (sicp utils table))

  (begin
    (define nil (list))
    (define false #f)
    (define true #t)

    (define (timeit f)
      (define (measure f start)
        (f)
        (- (runtime) start))
      (define (report n sum sqsum)
        (display n)
        (display "x -- ")
        (display (/ sum n))
        (display " +- ")
        (display (sqrt (/ (- sqsum (/ (sqr sum) n)) (- n 1))))
        (newline))
      (define (iter n sum sqsum end)
        (if (and (> n 3) (> (runtime) end))
            (report n sum sqsum)
            (let ((time (measure f (runtime))))
              (iter (+ n 1)
                    (+ sum time)
                    (+ sqsum (sqr time))
                    end))))
      (iter 0 0 0 (+ (runtime) 1e6)))

    (define (caar p) (car (car p)))
    (define (cadr p) (car (cdr p)))
    (define (cdar p) (cdr (car p)))
    (define (cddr p) (cdr (cdr p)))
    (define (caaar p) (car (car (car p))))
    (define (caadr p) (car (car (cdr p))))
    (define (caddr p) (car (cdr (cdr p))))
    (define (cdadr p) (cdr (car (cdr p))))
    (define (cddar p) (cdr (cdr (car p))))
    (define (cdddr p) (cdr (cdr (cdr p))))
    (define (cadddr p) (car (cdr (cdr (cdr p)))))

    (define (abs x) (if (< x 0) (- x) x))
    (define (inc n) (+ n 1))
    (define (dec n) (- n 1))

    (define (even? x) (= 0 (modulo x 2)))
    (define (>= a b) (or (< b a) (= a b)))
    (define (<= a b) (or (< a b) (= a b)))

    (define (modulo a b)
     (define (fix a b r)
      (cond ((and (< a 0) (> b 0)) (+ b r))
            ((and (> a 0) (< b 0)) (+ b r))
            (else r)))
     (fix a b (remainder a b)))

    (define (symbol<? a b)
      (< a b))

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

    (define (power base exponent)
      (if (< exponent 0)
          (error "exponent less than zero -- EXP" base exponent))
      (define (exp-iter a b n)
        (cond ((= n 0) a)
              ((even? n) (exp-iter a
                                   (* b b)
                                   (/ n 2)))
              (else (exp-iter (* a b)
                              b
                              (- n 1)))))
      (exp-iter 1 base exponent))


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

    (define (memq item x)
      (cond ((null? x) false)
            ((eq? item (car x)) x)
            (else (memq item (cdr x)))))

    (define (println . args)
      (if (null? args)
          (newline)
          (begin (display (car args))
                 (display " ")
                 (apply println (cdr args)))))

    (define (debug-print x)
      (println "DEBUG: " x)
      x)

    ;; ==========================================
    ;;   put and get into a global table
    ;;   (need those in exercise 2.73 and maybe later)
    ;; ==========================================

    (define (impl-table)
      (define table (make-table))

      (define (make-key op type)
        (cons op type))

      (define (put op type item)
        (set! table
              (insert (make-key op type)
                      item
                      table)))

      (define (get op type)
        (let ((record (lookup (make-key op type)
                              table)))
          (and record (record))))

      ; export public functions
      (list put get))

    (define put '())
    (define get '())
    (let ((put_get (impl-table)))
      (set! put (car put_get))
      (set! get (cadr put_get)))

    (define put-coercion '())
    (define get-coercion '())
    (let ((put_get (impl-table)))
      (set! put-coercion (car put_get))
      (set! get-coercion (cadr put_get)))))
