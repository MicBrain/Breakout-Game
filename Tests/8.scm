; Conses the smallest element of the input list
; with the result of applying the function to
; the input list with the smallest element removed.
(define (selection-sort input)
    (cond ((null? input) ())
          (#t (cons (smallest input)
                (selection-sort (del-1st (smallest input) input))))))

; This is just a wrapper over the smaller-than
; function.
(define (smallest input)
    (smaller-than input (car input)))

; Checks if any element is smaller than "save"
; and returns the smallest element or the "save"
; value passed in.
(define (smaller-than input save)
    (cond ((null? input) save)
          ((< (car input) save) (smaller-than (cdr input) (car input)))
          (#t (smaller-than (cdr input) save))))

; Deletes (non-destructively) the *first* instance
; of "element" from the "input" list. (The MIT-
; Scheme builtins all seem to delete *every*
; instance.)
(define (del-1st element input)
    (cond ((null? input) ())
          ((equal? element (car input))
            (cdr input))
          (#t (cons (car input) (del-1st element (cdr input))))))