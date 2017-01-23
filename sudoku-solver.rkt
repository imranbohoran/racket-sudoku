;;; Sodoku solver

#lang racket

(define (atom? x)
  (not (pair? x)))
  
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
(provide transform)
(provide solve)