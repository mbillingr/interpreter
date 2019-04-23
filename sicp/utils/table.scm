(define-library (sicp utils table)
  (export make-table insert lookup)

  (begin
    (define (make-record key value) (cons key value))
    (define (key record) (car record))
    (define (value record) (cdr record))

    (define (lookup given-key set-of-records)
      (cond ((null? set-of-records) false)
            ((equal? given-key (key (car set-of-records)))
             (car set-of-records))
            (else (lookup given-key (cdr set-of-records)))))

    (define (adjoin-set x set)
      (if (element-of-set? x set)
          set
          (cons x set)))

    (define (insert given-key value set-of-records)
      (define (iter remaining-set result)
        (cond ((null? remaining-set)
               (cons (make-record given-key value)
                     result))
              ((equal? given-key (key (car remaining-set)))
               (append (cons (make-record given-key value)
                           result)
                     (cdr remaining-set)))
              (else (iter (cdr remaining-set)
                          (cons (car remaining-set)
                                result)))))
      (iter set-of-records '()))

    (define (make-table)
      '())))
