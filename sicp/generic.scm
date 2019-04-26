(define-library (sicp generic)
  (export apply-generic attach-tag type-tag contents)

  (import (builtin core)
          (sicp utils))

  (begin
    (define (apply-generic op . args)
      (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
          (if proc
              (apply proc (map contents args))
              (error "No method for these types: APPLY-GENERIC"
                     (list op type-tags))))))

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
