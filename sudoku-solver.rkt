;;; Sodoku solver
;;;
;;; Imran Bohoran
;;;
;;;The algorithm
;;;• Repeatedly do the following:
;;;    Find a location containing a singleton set (a set containing just one number).
;;;      For every other set in the same row, the same column, or the same 3x3 box, remove that number (if present).       --> Done
;;;    Find a number in a set that does not occur in any other set in the same row (or column, or box).                    --> Done
;;;      Reduce that set to a singleton containing that one number.                                                        --> Done ; singletons-from-sets
;;;• Quit when every set is a singleton, or when no more numbers can be removed from any set.                              --> Done
;;;
;;;

#lang racket
;;;;
;;;; Utility functinos used for solving a sudoku grid.
;;;;

;; Checks if the a given value is an atom
(define (atom? x)
  (not (pair? x)))

;; Checks that a given value exists in the list provided.
(define (is-member? value listToCheck)
  (cond ((empty? listToCheck) #f)
        ((member value (car listToCheck)) #t)
        (else (is-member? value (cdr listToCheck))))
  )

;; Find the unique value of a given list against another list
(define (find-unique possibleUniques listToCheck)
    (cond ((empty? possibleUniques) possibleUniques)
          ((empty? listToCheck) '())
          ((list? possibleUniques) (cond ((is-member? (car possibleUniques) listToCheck) (find-unique (cdr possibleUniques) listToCheck))
                              (else (car possibleUniques))))
          ((atom? possibleUniques) (cond ((is-member? possibleUniques listToCheck) '())
                              (else possibleUniques))))
    )

;; Checks if a given list is "solved" as a sudoku row/grid/column
(define (is-solved? listToCheck)
  (define (check-cell listToCheck result)
    (cond ((equal? result #f) result)
          ((empty? listToCheck) result)
          ((memv 0 listToCheck) #f)
          ((list? (car listToCheck)) (check-cell listToCheck #f))
          (else (check-cell (cdr listToCheck) result))
    )
  )
  (check-cell listToCheck #t))


;; Extracts a singleton from a given list
(define (extract-singleton listToExtractFrom)
  (define
    (process-singletons in out)
      (cond ((empty? in) out)
            ((atom? (car in)) (process-singletons (cdr in) (cons (car in) out)))
            (else (process-singletons (cdr in) out))
      )
    )
  (process-singletons listToExtractFrom '()))

;; Removes all given singletons from a list
(define (prune-sets singletons listToProcess)
  (define (process-sets singletons values pruned)
    (cond ((empty? values) pruned)
          ((atom? (car values)) (process-sets singletons (cdr values) (append pruned (list (car values)))))
          ((list? (car values)) (process-sets singletons (cdr values) (append pruned (list (remove* singletons (car values))))))
    )
  )
  (process-sets singletons listToProcess '())
  )

;; Extracts all non-singleton sets from a given list
(define (extract-sets listToProcess)
  (define
    (process-sets in out)
      (cond ((empty? in) out)
            ((list? (car in)) (process-sets (cdr in) (append out (list (car in)))))
            (else (process-sets (cdr in) out))
      )
    )
  (process-sets listToProcess '()))


;; Extracts singletons from sets when there are unique values
(define (singletons-from-sets listToProcess)
  (define (make-singletons values updated)
    (cond ((empty? values) updated)
          ((atom? (car values)) (make-singletons (cdr values) (append updated (list (car values)))))
          (else (let ((unique-value '()))
                  (set! unique-value (find-unique (car values) (append (extract-sets updated) (extract-sets (cdr values)))))
                  (cond ((number? unique-value) (make-singletons (cdr values) (append updated (list unique-value))))
                        (else (make-singletons (cdr values) (append updated (list (car values)))))))))          
    )
  (make-singletons listToProcess '())
 )

;; Normalises the given list of values so that if a single element list is a member of a list they
;; get converted to an atom.
(define (normalise values results)
  (cond ((empty? values) results)
        ((atom? (car values)) (normalise (cdr values) (append results (list (car values)))))
        ((list? (car values)) (normalise (cdr values) (append results (car values))))
        )
  )

;;;;;;;
;;;;;;; Main functions used for solving the sudoku grid  ;;;;;;
;;;;;;;

;; Transforms a given matrix so that all 0s are replaced with a list of values containing 1 2 3 4 5 6 7 8 9
(define (transform matrix)
  (map (lambda (x)
         (cond ((list? x) (transform x))
               ((and (atom? x) (zero? x)) (list 1 2 3 4 5 6 7 8 9))
               (else x)))
       matrix))

;; Solves a simple sudoku puzzle
(define (solve matrix)
  (define (attempt-solving values)
    (cond ((is-solved? values) '())
          (else (process-row values))
         )
    )
  (define (process-row values)
    (normalise (singletons-from-sets (prune-sets (extract-singleton values) values)) '())
    )
  
  (map attempt-solving (transform matrix))
)

(provide atom?)
(provide is-member?)
(provide is-solved?)
(provide find-unique)
(provide extract-singleton)
(provide prune-sets)
(provide extract-sets)
(provide singletons-from-sets)
(provide normalise)
(provide transform)
(provide solve)
