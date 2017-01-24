;;; Sodoku solver

#lang racket

(define (atom? x)
  (not (pair? x)))

(define (extract-singleton values)
  (define
    (process-singletons in out)
      (cond ((empty? in) out)
            ((atom? (car in)) (process-singletons (cdr in) (cons (car in) out)))
            (else (process-singletons (cdr in) out))
      )
    )
  (process-singletons values '()))
  
(define (transform matrix)
  (map (lambda (x)
         (cond ((list? x) (transform x))
               ((and (atom? x) (zero? x)) (list 1 2 3 4 5 6 7 8 9))
               (else x)))
       matrix))

(define (solve matrix)
  "Not yet Implemented"
)

(provide atom?)
(provide extract-singleton)
(provide transform)
(provide solve)