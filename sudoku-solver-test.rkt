;;; Tests for the sudoku solver

#lang racket
 
(require rackunit
         "sudoku-solver.rkt")

(define sudoku-solver-tests
  (test-suite
   "Tests the sudoku solver"

   (check-equal? (atom? 2) #t "Should be an atom")
   (check-equal? (atom? '(2 3 4)) #f "Should not be an atom. It's a list")
  )
)

(require rackunit/text-ui)
 
(run-tests sudoku-solver-tests)