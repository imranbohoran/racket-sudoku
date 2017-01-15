;;; Tests for the sudoku solver

#lang racket
 
(require rackunit
         "sudoku-solver.rkt")

(define sudoku-solver-tests
  (test-suite
   "Tests the sudoku solver"

   (check-equal? (atom? 2) #t "Should be an atom")
   (check-equal? (atom? '(2 3 4)) #f "Should not be an atom. It's a list")

   (test-case
    "List has length 4 and all elements even"
    (let ([lst '(2 4 6 8)])
      (check = (length lst) 4)
      (for-each
        (lambda (elt)
          (check-pred even? elt))
      lst)))
   (test-case
    "0s should be replaced"
    (let ([matrix '(2 3 4 0 5)])
      (check-equal? (transform matrix) '(2 3 4 '(1 2 3 4 5 6 7 8 9) 5)
   )))
  )
)

(require rackunit/text-ui)
 
(run-tests sudoku-solver-tests)