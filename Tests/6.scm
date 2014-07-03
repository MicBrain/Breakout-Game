;Function to call how many times a given element occurs in a list
(define (countOccurances element L compare?)
  (define (countOccurancesHelper element L compare? result)
    (if (null? L) result
        (if (compare? element (car L))
            (countOccurancesHelper element (cdr L) compare? (+ result 1))
            (countOccurancesHelper element (cdr L) compare? result)
        )
    )
  )
  (countOccurancesHelper element L compare? 0)
)

;Removing some element from list
(define (removeElement L el compare?)
  (filter (lambda (x) (not (compare? x el))) L)
)

;Make frequence list e.g. ((a.4) (b.1) (c.2))
(define (makeFrequencyList L compare?)
  (define (makeFrequencyListHelper L compare? result)
    (if (null? L)
        result
        (makeFrequencyListHelper (removeElement L (car L) compare?) compare? (append result (list (cons (list (car L)) (countOccurances (car L) L compare?)))))
    )
  )
  (makeFrequencyListHelper L compare? '())
)



(define (contains? el L compare?)
  (if (null? L)
      #f
      (if (compare? el (car L))
          #t
          (contains? el (cdr L) compare?)
      )
  )
)

(define (makeEmptyCodesList L compare?)
  (define (makeEmptyCodesListHelper L compare? result)
    (if (null? L)
        result
        (if (contains? (cons (car L) (list)) result compare?)
            (makeEmptyCodesListHelper (cdr L) compare? result)
            (makeEmptyCodesListHelper (cdr L) compare? (append result (list (cons (car L) (list)))))
        )
    )
  )
  (makeEmptyCodesListHelper L compare? (list ))
)

;(define emptyCode (makeEmptyCodesList '(a t d g) equal?))
;(makeEmptyCodesList '("asda" 123 "maina") equal?)
;(define freq (makeFrequencyList '(1 2 3 2 1 5 4) equal?))
;freq

(define (findMinOcc freqList)
  (define (takeLess one two)
    (if (< (cdr one) (cdr two))
        one
        two
    )
  )
  (define (findMinOccHelper freqList min)
    (if (null? (cdr freqList))
        (takeLess min (car freqList))
        (findMinOccHelper (cdr freqList) (takeLess min (car freqList)))
    )
  )
  (findMinOccHelper freqList (car freqList))
)
;(findMinOcc freq)


(define (mergeLeast freqList compare?)
  (define first (findMinOcc freqList))
  (define freqList2 (removeElement freqList first compare?))
  (define second (findMinOcc freqList2))
  (define freqList3 (removeElement freqList2 second compare?))
  (append freqList3 (list (cons (append (car first) (car second)) (+ (cdr first) (cdr second)))))
  ;(cons (append freqList3 (list (cons (append (car first) (car second)) (+ (cdr first) (cdr second))))) (cons first second))
 )
;(mergeLeast freq)
;(mergeLeast (mergeLeast freq))
;(mergeLeast (mergeLeast (mergeLeast freq)))
;(mergeLeast (mergeLeast (mergeLeast (mergeLeast freq))))

(define (takeTwoLeastFreqs freqList compare?)
  (define first (findMinOcc freqList))
  (define freqList2 (removeElement freqList first compare?))
  (define second (findMinOcc freqList2))
  (cons first second)
)

;(takeTwoLeastFreqs (mergeLeast freq))

(define (addBit codes element bit compare?)
  (define (addBitHelper codes element bit compare? result)
    (if (null? codes)
        result
        (if (compare? element (caar codes))
            (addBitHelper (cdr codes) element bit compare? (append result (list (cons (caar codes) (append (list bit) (cdar codes))))))
            (addBitHelper (cdr codes) element bit compare? (append result (list (car codes))))
        )
    )
  )
  (addBitHelper codes element bit compare? (list))
)

(define (addBitToMany codes listOfElements bit compare?)
  (if (null? listOfElements)
      codes
      (addBitToMany (addBit codes (car listOfElements) bit compare?) (cdr listOfElements) bit compare?)
  )
)

(define (getCodes L compare?)
  (define (getCodesHelper freqList codes compare?)
    (if (null? (cdr freqList))
        codes
        ;(getCodesHelper (mergeLeast freqList) (addBitToMany codes (caar (takeTwoLeastFreqs freqList)) 0 compare?) compare?)
        (getCodesHelper (mergeLeast freqList compare?) (addBitToMany (addBitToMany codes (caar (takeTwoLeastFreqs freqList compare?)) 0 compare?) (cadr (takeTwoLeastFreqs freqList compare?)) 1 compare?) compare?)
    )
  )
  (getCodesHelper (makeFrequencyList L compare?) (makeEmptyCodesList L compare?)  compare?)
)
;(getCodes '(a a a a b b c d a) equal?)


(define (getSymbolCode symbol codes compare?)
  (if (compare? (caar codes) symbol)
    (cdar codes)
    (getSymbolCode symbol (cdr codes) compare?)
  )
)

(define (turnIntoBits codes symbols compare?)
  (define (turnIntoBitsHelper codes symbols result compare?)
    (if (null? symbols)
        result
        (turnIntoBitsHelper codes (cdr symbols) (append result (getSymbolCode (car symbols) codes compare?)) compare?)
     )    
  )
  (turnIntoBitsHelper codes symbols '() compare?)
)

(define (encode L compare?)
  (define freqList (makeFrequencyList L compare?))
  (define codes (getCodes L compare?))
  (define encoded (turnIntoBits codes L compare?))
  (list freqList codes encoded)
)



(define (removeFirstBits codes)
  (define (removeFirstBitsHelper codes result)
    (if (null? codes)
        result
        (removeFirstBitsHelper (cdr codes) (append result (list (cons (caar codes) (cdr (cdar codes))))))
    )
  )
  (removeFirstBitsHelper codes '())
)
    
  
#|(define (decode E)
  (define codes (cadr E))
  (define bits (cddr E))
  (define (decodeHelper originalCodes currCodes bits result)
    (if (null? bits)
        result
        (if (and (equal? (car bits) (car (cdar currCodes))) (null? (cdr (cdar currCodes))))
            (decodeHelper originalCodes originalCodes (cdr bits) (append result (list (caar currCodes))))
            (decodeHelper originalCodes (removeFirstBits (filter (lambda (x) (equal? (car bits) (cadr x))) currCodes)) (cdr bits) result)
        )   
        
    )
  )
  (decodeHelper codes codes bits (list))
)|#
(define (decode E)
  (define codes (cadr E))
  ;(define bits (car (cddr E)))
  (define bits (append (car (cddr E)) (list 1)))
  (define (decodeHelper originalCodes currCodes bits result)
    (if (null? bits)
        result
        (if (null? (cdar currCodes))
            (decodeHelper originalCodes originalCodes bits (append result (list (caar currCodes))))
            (decodeHelper originalCodes (removeFirstBits (filter (lambda (x) (equal? (car bits) (cadr x))) currCodes)) (cdr bits) result)
        )
    )
  )
  (decodeHelper codes codes bits '())
)

(define li '(a a a a b b c d a))
(define liCoded (encode li equal?))
liCoded
(decode liCoded)

(define li2 '(a a '(b b) '(1 2 3) '(1 2 3)))
(define li2Coded (encode li2 eq?))
li2Coded
(decode li2Coded)

(define li2CodedEqual (encode li2 equal?))
li2CodedEqual
(decode li2CodedEqual)
;(cadr liCoded)
;(removeFirstBits (cadr liCoded))