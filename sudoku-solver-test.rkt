;;; Tests for the sudoku solver

#lang racket
 
(require rackunit
         "sudoku-solver.rkt")

(define sudoku-solver-tests
  (test-suite
   "Test suite for the sudoku solver"
   (test-suite
    "Tests the utilities used for the sudoku solver"
    (test-case
     "an atom should be identified as an atom"
     (check-equal? (atom? 2) #t "Should be an atom")
    )
    (test-case
     "A list should not be identified as an atom"
     (check-equal? (atom? '(2 3 4)) #f "Should not be an atom. It's a list")
    )
    (test-case
     "All singletons should be extracted into a single list"
     (check-equal? (extract-singleton '(1 2 (1 2 3 4 5 6 7 8 9) 5 8 9 (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) 6)) '(6 9 8 5 2 1))
    )
   )
   (test-suite
   "Tests the sudoku solver transform function"
   (test-case
    "0s should be replaced"
    (let ([matrix '(2 3 4 0 5 6 1 7 8 9)])
      (check-equal? (transform matrix) '(2 3 4 (1 2 3 4 5 6 7 8 9) 5 6 1 7 8 9)
   )))
   (test-case
    "0s should be replaced across the whole matrix"
    (let ([matrix '((2 3 4 0 5 6 1 7 8) (1 3 0 0 1 6 7 8 9) (0 2 3 4 0 6 7 8 9))])
      (check-equal? (transform matrix) '((2 3 4 (1 2 3 4 5 6 7 8 9) 5 6 1 7 8) (1 3 (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) 1 6 7 8 9) ((1 2 3 4 5 6 7 8 9) 2 3 4 (1 2 3 4 5 6 7 8 9) 6 7 8 9))
   )))
  )
  (test-suite
   "Tests the sudoku solver solver function"
;   (test-case
;    "Solve should remove singletons from lists"
;    (let ([matrix '(2 3 4 5 6 (1 2 3 4 5 6 7 8 9) 1 7 9)])
;      (check-equal? (solve matrix) '(2 3 4 5 6 8 1 7 9)
;   )))
  )
 )
)

(require rackunit/text-ui)
(run-tests sudoku-solver-tests)