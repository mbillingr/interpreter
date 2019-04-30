(define-library (sicp generic)
  (export apply-generic apply-generic-method attach-tag type-tag contents print)

  (import (builtin core)
          (sicp utils))

  (begin
    (define (print x) (apply-generic 'print x))

    (define (apply-generic op . args)
      ;(println op args)
      (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
          (if proc
              (apply proc (map contents args))
              (error "No method for these types: APPLY-GENERIC"
                     (list op type-tags))))))

    (define (apply-generic-method op self . args)
      (let ((proc (get op (type-tag self))))
        (if proc
            (apply proc (contents self) args)
            (error "No method for these types: APPLY-GENERIC-METHOD"
                   (list op (type-tag self))))))

    (define (attach-tag type-tag contents)
      (cond ((and (eq? type-tag 'scheme-number)
                  (number? contents))
             contents)
            (else (cons type-tag contents))))

    (define (type-tag datum)
      (cond ((pair? datum) (car datum))
            ((number? datum) 'scheme-number)
            (else (error "Bad tagged datum: TYPE-TAG" datum))))

    (define (contents datum)
      (cond ((pair? datum) (cdr datum))
            ((number? datum) datum)
            (else (error "Bad tagged datum: CONTENTS" datum))))))
