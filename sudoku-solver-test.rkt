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
     "Should return true for an existing member in a list"
     (check-equal? (is-member? 2 '((1 2) (3 4) (5 6))) #t "Should return true")
     )
    (test-case
     "Should return true for an existing member in a list - 2"
     (check-equal? (is-member? 3 '((1 2) (3 4) (5 6))) #t "Should return true")
     )
    (test-case
     "Should return false for a non-existing member in a list"
     (check-equal? (is-member? 7 '((1 2) (3 4) (5 6))) #f "Should return false")
     )
    (test-case
     "All singletons should be extracted into a single list"
     (check-equal? (extract-singleton '(1 2 (1 2 3 4 5 6 7 8 9) 5 8 9 (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) 6)) '(6 9 8 5 2 1))
    )
    (test-case
     "Should remove all singletons from sets"
     (check-equal? (prune-sets '(6 9 8 5 2 1) '(1 2 (1 2 3 4 5 6 7 8 9) 5 8 9 (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) 6)) '(1 2 (3 4 7) 5 8 9 (3 4 7) (3 4 7) 6))
     )
    (test-case
     "Should extract all non-singleton sets"
     (check-equal? (extract-sets '(1 2 (1 2 3 4 5 6 7 8 9) 5 8 9 (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) 6)) '((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)))
     )
     (test-case
     "Should return empty list if no non-singleton sets found"
     (check-equal? (extract-sets '(1 2 5 8 9 6)) '())
     )
    (test-case
     "Should return unique value when unique value exists"
     (check-equal? (find-unique '(1 2) '((3 4 7) (2 9) (3 9) (7 9) (3 7 9))) 1)
     )
    (test-case
     "Should return empty value when unique value does not exists"
     (check-equal? (find-unique '(3 2) '((3 4 7) (2 9) (3 9) (7 9) (3 7 9))) '())
     )
    (test-case
     "Should return empty value when uniqueness checked against an empty list"
     (check-equal? (find-unique '(3 2) '()) '())
     )

    (test-case
     "Should create singletons from sets if they have values unique to them"
     (check-equal? (singletons-from-sets '(1 2 (3 4 7) 5 6 (8 9) (3 9) (7 9) (3 7 9))) '(1 2 4 5 6 8 (3 9) (7 9) (3 7 9)))
     )

    (test-case
     "Should be satisfied when every element is a singleton"
     (check-equal? (is-solved? '(1 2 3 (4 7 8) 5 6)) #f "Should return false")
     )

    (test-case
     "Should not be satisfied when at least one element is still not a singleton"
     (check-equal? (is-solved? '(1 2 3 4 5 6)) #t "Should return true")
     )
    
    (test-case
     "Should not be satisfied when at least one element is still 0"
     (check-equal? (is-solved? '(1 2 3 4 5 6 0)) #f "Should return false")
     )

    (test-case
     "Should normalise any singleton represented as a list to an atom"
     (check-equal? (normalise '(1 2 3 4 (5) 6 7 8 9) '()) '(1 2 3 4 5 6 7 8 9)) 
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
   (test-case
    "Solve should remove singletons from lists"
    (let ([matrix '((2 3 4 5 6 0 1 7 9))])
      (check-equal? (solve matrix) '((2 3 4 5 6 8 1 7 9))
   )))
   (test-case
    "Solve should remove singletons from matrix"
    (let ([matrix '((2 3 4 5 6 0 1 7 9) (1 2 5 4 8 3 0 9 7) (3 1 2 6 0 4 9 7 8))])
      (check-equal? (solve matrix) '((2 3 4 5 6 8 1 7 9) (1 2 5 4 8 3 6 9 7) (3 1 2 6 5 4 9 7 8))
   )))
  )
 )
)

(require rackunit/text-ui)
(run-tests sudoku-solver-tests)