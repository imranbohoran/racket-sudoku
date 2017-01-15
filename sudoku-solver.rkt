;;; Sodoku solver

#lang racket

(define (atom? x)
  (not (pair? x)))

(define (transform-rule x)
  (if (zero? x)
      (list 1 2 3 4 5 6 7 8 9)
      x
  ))

(define (transform matrix)
  (map transform-rule matrix))

(define (solve matrix)
  "Not yet Implemented"
)

(provide atom?)
(provide transform)
