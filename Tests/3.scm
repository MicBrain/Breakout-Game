;;; Максимальное значение Int
(define MaxInt 4294967087)

;;; Вычисление длины списка
(define (ch-length ch acc)
  (cond ((null? ch) acc)
        (else (ch-length (cdr ch) (+ acc 1)))))
;;;Пример вызова
;;;(ch-length '(1 2 3) 0)

;;; Случайное вещественное значение
(define (random-double range-min range-max)
  (+ range-min (* (- range-max range-min) (/ (random MaxInt) MaxInt))))
;;;Пример вызова
;;;(random-double 1.1 2.2)

;;; Минимальное значение
(define (minimum val1 val2)
  (cond ((< val1 val2) val1)
        (else val2)))
;;;Пример вызова
;;;(minimum 1.1 2.2)

;;; Максимальное значение
(define (maximum val1 val2)
  (cond ((> val1 val2) val1)
        (else val2)))
;;;Пример вызова
;;;(maximum 1.1 2.2)

;;; Плоский кроссовер
(define (float-crossover ch1 ch2 out)
  (cond ((not (equal? (ch-length ch1 0) (ch-length ch2 0))) (display "Wrong chromosome size!!!"))
        ((or (null? ch1) (null? ch2)) out)
        (else (float-crossover (cdr ch1) (cdr ch2) (append out (list (random-double (car ch1) (car ch2))))))))
;;;Пример вызова
;;;(float-crossover '(6.0 4.0 8.0) '(3.0 8.0 9.0) '())

;;; Арифметический кроссовер
(define (arithmetical-crossover ch1 ch2 N out1 out2)
  (cond ((not (equal? (ch-length ch1 0) (ch-length ch2 0))) (display "Wrong chromosome size!!!"))
        ((or (< N 0) (> N 1)) (display "Wrong coefficient N!!!"))
        ((or (null? ch1) (null? ch2)) (list out1 out2))
        (else (arithmetical-crossover (cdr ch1) (cdr ch2) N 
                                      (append out1 (list (+ (* N (car ch1)) (* (- 1.0 N) (car ch2))))) 
                                      (append out2 (list (+ (* N (car ch2)) (* (- 1.0 N) (car ch1)))))))))
;;;Пример вызова
;;;(arithmetical-crossover '(6.0 4.0 8.0) '(3.0 8.0 9.0) 0.5 '() '())

;;; BLX-a кроссовер
(define (blx-a-crossover ch1 ch2 A out)
  (cond ((not (equal? (ch-length ch1 0) (ch-length ch2 0))) (display "Wrong chromosome size!!!"))
        ((or (null? ch1) (null? ch2)) out)
        (else (blx-a-crossover (cdr ch1) (cdr ch2) A 
                               (append out (list (random-double 
                                                  (- (minimum (car ch1) (car ch2)) (* (- (maximum (car ch1) (car ch2)) (minimum (car ch1) (car ch2))) A)) 
                                                  (+ (maximum (car ch1) (car ch2)) (* (- (maximum (car ch1) (car ch2)) (minimum (car ch1) (car ch2))) A)))))))))
;;;Пример вызова
;;;(blx-a-crossover '(6.0 4.0 8.0) '(3.0 8.0 9.0) 0.5 '())

;;; Линейный кроссовер
(define (linear-crossover ch1 ch2 out1 out2 out3)
  (cond ((not (equal? (ch-length ch1 0) (ch-length ch2 0))) (display "Wrong chromosome size!!!"))
        ((or (null? ch1) (null? ch2)) (list out1 out2 out3))
        (else (linear-crossover (cdr ch1) (cdr ch2) 
                                (append out1 (list (/ (+ (car ch1) (car ch2)) 2.0))) 
                                (append out2 (list (/ (- (* 3.0 (car ch1)) (car ch2)) 2.0))) 
                                (append out3 (list (/ (+ (- 0.0 (car ch1)) (* 3.0 (car ch2))) 2.0)))))))
;;;Пример вызова
;;;(linear-crossover '(6.0 4.0 8.0) '(3.0 8.0 9.0) '() '() '())

;;; Мутация гена
(define (gene-mutation gene percent)
  (cond ((or (< percent 0) (> percent 100)) (display "Wrong percentage of mutation!!!"))
        (else (random-double (- gene (* gene (/ percent 100))) (+ gene (* gene (/ percent 100)))))))
;;;Пример вызова
;;;(gene-mutation 5.0 10)

;;;Мутация хромосомы
(define (ch-mutation ch chance percent out)
  (cond ((null? ch) out)
        ((or (< chance 0) (> chance 100)) (display "Wrong chance!!!"))
        ((equal? 0 (random (- 101 chance))) (ch-mutation (cdr ch) chance percent (append out (list (gene-mutation (car ch) percent)))))
        (else (ch-mutation (cdr ch) chance percent (append out (list (car ch)))))))
;;;Пример вызова
;;;(ch-mutation '(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0) 70 20 '())

;;;Мутация популяции
(define (population-mutation population chance percent out)
  (cond ((null? population) out)
        (else (population-mutation (cdr population) chance percent (append out (list (ch-mutation (car population) chance percent '())))))))
;;;Пример вызова
;;;(population-mutation (list '(1.0 2.0 3.0) '(4.0 5.0 6.0) '(7.0 8.0 9.0)) 50 20 '())