;;; Sodoku solver
;;;
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

;; Utility functinos used for solving a sudoku grid.
(define (atom? x)
  (not (pair? x)))

(define (is-member? x values)
  (cond ((empty? values) #f)
        ((member x (car values)) #t)
        (else (is-member? x (cdr values))))
  )

(define (find-unique comp values)
    (cond ((empty? comp) comp)
          ((empty? values) '())
          ((list? comp) (cond ((is-member? (car comp) values) (find-unique (cdr comp) values))
                              (else (car comp))))
          ((atom? comp) (cond ((is-member? comp values) '())
                              (else comp))))
    )

(define (is-solved? values)
  (define (check-cell values result)
    (cond ((equal? result #f) result)
          ((empty? values) result)
          ((memv 0 values) #f)
          ((list? (car values)) (check-cell values #f))
          (else (check-cell (cdr values) result))
    )
  )
  (check-cell values #t))

         
(define (extract-singleton values)
  (define
    (process-singletons in out)
      (cond ((empty? in) out)
            ((atom? (car in)) (process-singletons (cdr in) (cons (car in) out)))
            (else (process-singletons (cdr in) out))
      )
    )
  (process-singletons values '()))

(define (prune-sets singletons values)
  (define (process-sets singletons values pruned)
    (cond ((empty? values) pruned)
          ((atom? (car values)) (process-sets singletons (cdr values) (append pruned (list (car values)))))
          ((list? (car values)) (process-sets singletons (cdr values) (append pruned (list (remove* singletons (car values))))))
    )
  )
  (process-sets singletons values '())
  )

(define (extract-sets values)
  (define
    (process-sets in out)
      (cond ((empty? in) out)
            ((list? (car in)) (process-sets (cdr in) (append out (list (car in)))))
            (else (process-sets (cdr in) out))
      )
    )
  (process-sets values '()))

(define (singletons-from-sets values)
  (define (make-singletons values updated)
    (cond ((empty? values) updated)
          ((atom? (car values)) (make-singletons (cdr values) (append updated (list (car values)))))
          (else (let ((unique-value '()))
                  (set! unique-value (find-unique (car values) (append (extract-sets updated) (extract-sets (cdr values)))))
                  (cond ((number? unique-value) (make-singletons (cdr values) (append updated (list unique-value))))
                        (else (make-singletons (cdr values) (append updated (list (car values)))))))))          
    )
  (make-singletons values '())
 )

(define (normalise values results)
  (cond ((empty? values) results)
        ((atom? (car values)) (normalise (cdr values) (append results (list (car values)))))
        ((list? (car values)) (normalise (cdr values) (append results (car values))))
        )
  )

;; Main functions used for solving the sudoku grid
(define (transform matrix)
  (map (lambda (x)
         (cond ((list? x) (transform x))
               ((and (atom? x) (zero? x)) (list 1 2 3 4 5 6 7 8 9))
               (else x)))
       matrix))

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
