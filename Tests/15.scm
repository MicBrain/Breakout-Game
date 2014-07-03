(load "prelude.scm")
(load "pmatch.scm")

(define POT-simple
  '(POT ((main) Major Turnaround C (chord C 1) (brick GenII G 1)
          (brick Straight-Approach C 2))))

;;; In our grammar, this looks like:

(define POT-our-way
  '(POT_main -> C GenII Straight-Approach))

;;; This function gets the key of a variant of a brick by using pmatch to navigate the bricks definitions

(define get-key
  (lambda (name table)
    (pmatch table
      [(,A . ,rest) (guard (eqv? A (pre name)))
       (pmatch rest
         [((,var) ,mode ,type ,key . ,rest) (guard (eqv? var (post name)))
          (key)])])))

(define pre
  (lambda (name)
    (let ([list (string->list (symbol->string name))])
      (cond
;;; How does -> work with cond?
        [(memv '_ list) (difference list (memv '_ list))]
        [else (error 'here "uh oh")]))))

(define post
  (lambda (name)
    (let ([list (string->list (symbol->string name))])
      (cond
        [(memv '_ list) -> cdr]
        [else (error 'there "oh no")]))))

(define brick
  (lambda (name key duration)
    '(brick ,name ,key ,duration)))

(define chord
  (lambda (name duration)
    '(chord ,name ,duration)))

; (move-note 'A 2) => B
; (move-note 'A$ 2) => C
; (move-note 'Ab 2) => Bb
; Decision: always return flat version of notes
(define move-note
  ; interval in semi-tones
  (lambda (note interval)
    '???))

; (sharpen 'Ab) => G$
; (sharpen 'A$) => A$
; (sharpen 'A) => A
(define sharpen
  (lambda (note)
    '???))
(define flatten
  (lambda (note)
    '???))

(define transpose-chord
  (lambda (chord interval)
    '???))

(define transpose-block
  (lambda (block interval)
    (block-match block
      [,chord (transpose-chord chord interval)]
      [(,name ,qualifier ,mode ,type ,key ,block*)
       ; Question: do we really want to both change the key of this
       ; brick and process the subblocks recursively?
       (let ([block* (map block*)]))])))

(define transpose
  (lambda (brick-name target-key)
    (let ([brick (lookup-brick brick-name)])
      (brick-match brick
        [(,name ,qualifier ,mode ,type ,key ,block*)
         (let ([interval (find-interval key target-key)])
           (map (lambda (block) (transpose-block block interval)) block*))]))))

(test 'transpose-1
  (transpose 'POT_main 'A)
  '((chord A 1) (brick GenII E 1) (brick Straight-Approach A 2)))