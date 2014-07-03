
(define pontifex-rstate (random-state-from-platform))

;; Helper functions

(define (delete-from-index n l)
  (cond ((null? l)
         '())
        ((> n 0)
         (cons (car l)
               (delete-from-index (1- n) (cdr l))))
        (else
         (cdr l))))

(define (zip-with f l1 l2)
  (cond ((null? l1)
         '())
        ((null? l2)
         '())
        (else (cons (f (car l1) (car l2))
                    (zip-with f (cdr l1) (cdr l2))))))

(define (pair-each a l)
  (cond ((null? l) '())
        (else (cons (cons a (car l))
                    (pair-each a (cdr l))))))

(define (xzip l1 l2)
  (cond ((null? l1) '())
        (else (cons (pair-each (car l1) l2)
                    (xzip (cdr l1) l2)))))

(define (suit-face-pair-to-symbol p)
  (let ((suit (symbol->string (car p)))
        (face (symbol->string (cdr p))))
    (string->symbol (string-concatenate (list face "-of-" suit)))))

(define (vector-swap! v n1 n2)
  (cond ((equal? n1 n2) '())
        (else (let ((x (vector-ref v n1)))
                (vector-set! v n1 (vector-ref v n2))
                (vector-set! v n2 x)))))

(define (shuffle l)
  (let ((l2 (list->vector l))
        (sz (length l))
        (n 0))
    (for-each (lambda (z)
                (vector-swap! l2 n (random sz pontifex-rstate))
                (set! n (+ n 1)))
              l)
    (vector->list l2)))

;; Card functions

(define faces '(ace
                two
                three
                four
                five
                six
                seven
                eight
                nine
                ten
                jack
                queen
                king))

(define suits '(clubs
                diamonds
                hearts
                spades))

(define special-cards '(joker-a
                        joker-b))

(define (make-fresh-deck)
  (append (map suit-face-pair-to-symbol
               (concatenate (xzip suits faces)))
          special-cards))

(define *clean-deck* (make-fresh-deck))

(define (get-new-shuffled-deck)
  (shuffle *clean-deck*))

;;Core stuff

(define (pontifex-sanitise c)
  (let ((index (list-index (cut equal? c <>) pontifex-char-list)))
    (if (eq? #f index)
        #\X
        c)))

(define pontifex-char-list
  (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(define (pontifex-char-add c k)
  (let ((ci (list-index (cut equal? c <>) pontifex-char-list)))
    (list-ref pontifex-char-list (modulo (+ ci k) 26))))

(define (pontifex-char-sub c k)
  (let ((ci (list-index (cut equal? c <>) pontifex-char-list)))
    (list-ref pontifex-char-list (modulo (- ci k) 26))))

(define (pontifex-value card)
  (cond ((equal? 'joker-a card) 53)
        ((equal? 'joker-b card) 53)
        (else (1+ (list-index (cut equal? card <>) *clean-deck*)))))

(define (jump-card-forwards card steps deck)
  (let* ((oldpos (list-index (cut equal? card <>) deck))
         (newpos (modulo (+ 1 steps oldpos) 54))
         (d2 (delete-from-index oldpos deck))
         (left (take d2 newpos))
         (right (drop d2 newpos)))
    (if (zero? newpos)
        (concatenate (list d2
                           (list card)))
        (concatenate (list left
                           (list card)
                           right)))))

(define (joker? c)
  (or (equal? c 'joker-a)
      (equal? c 'joker-b)))

(define (triple-joker-cut deck)
  (let* ((a (list-index (cut equal? 'joker-a <>) deck))
         (b (list-index (cut equal? 'joker-b <>) deck))
         (fst-j (min a b))
         (lst-j (max a b))
         (head (take deck fst-j))
         (tail (drop deck (1+ lst-j)))
         (mid (drop (take deck (1+ lst-j)) fst-j)))
    (concatenate (list tail mid head))))

(define (count-cut deck)
  (let ((bottom (last deck)))
    (if (joker? bottom)
        deck
        (let* ((i (pontifex-value bottom))
               (head (take deck i))
               (tail (drop-right (drop deck i) 1)))
          (concatenate (list tail
                             head
                             (list bottom)))))))

(define (output-card deck)
  (let ((i (1- (pontifex-value (car deck)))))
    (list-ref deck i)))

(define (card-to-keystream-value c)
  (modulo (pontifex-value c) 26))

(define (deck-encryption-step deck)
  (let* ((step1 (jump-card-forwards 'joker-a 1 deck))
         (step2 (jump-card-forwards 'joker-b 2 step1))
         (step3 (triple-joker-cut step2))
         (step4 (count-cut step3)))
    step4))

(define (keystream-value-from-deck deck)
  (card-to-keystream-value (output-card deck)))

(define (get-keystream-by-length n deck)
  (if (zero? n)
      '()
      (let ((d2 (deck-encryption-step deck)))
        (cons (keystream-value-from-deck d2)
              (get-keystream-by-length (1- n) d2)))))

;; The Outwards-facing functions

(define pontifex-random-key get-new-shuffled-deck)

(define (pontifex-encrypt plaintext deck)
  (let* ((keytext (get-keystream-by-length (string-length plaintext) deck))
         (pt-lst (string->list (string-upcase plaintext)))
         (pt-lst-sanitised (map pontifex-sanitise pt-lst)))
    (list->string (zip-with pontifex-char-add pt-lst-sanitised keytext))))

(define (pontifex-decrypt ciphertext deck)
  (let ((keytext (get-keystream-by-length (string-length ciphertext) deck))
        (ct-lst (string->list (string-upcase ciphertext))))
    (list->string (zip-with pontifex-char-sub ct-lst keytext))))