; Funciones que implementan algunos algoritmos de búsqueda sin 
; información.
;
; Autor: Santiago Enrique Conant Pablos, Junio 2006

; Funcion de interface para la búsqueda en anchura
(define breadth-first 
  (lambda (edo-inicial)
    (breadth (list (list edo-inicial 0 (list (list 'Inicio edo-inicial)))))
    ))

; Función auxiliar que implementa la búsqueda en anchura.
(define breadth 
  (lambda (candidatos)
    (if (null? candidatos) 
        #f
        (let* ((nodo (car candidatos))
               (costo (cadr nodo))
               (ruta (caddr nodo)))
          (if (meta? nodo)
              (begin
                (display "Solucion: ")
                (display (reverse ruta))
                (newline)
                (display "Costo: ")
                (display costo)
                )
              (breadth (append (cdr candidatos) 
                               (elimina-ciclos (expande nodo)))))))
          ))

; Expande el estado contenido en un nodo.
; Requiere de la definición de la función sucesores, la cual regresa
; la lista de sucesores (((op1 suc1) cto1) ((op2 suc2) cto2)...), donde 
; op es una acción legal a ejecutar a partir del estado contenido en
; el nodo, suc es el estado resultante de op, y cto es el costo de
; ejecución de op a partir del nodo.
(define expande 
  (lambda (nodo)
    (let* ((ruta (caddr nodo))
           (costo (cadr nodo)))
      (display "Expande:")
      (display (reverse ruta))
      (newline)
      (newline)
      (map (lambda (suc)
             (list (cadar suc)
                   (+ costo (cadr suc))
                   (cons (car suc) ruta)))
           (sucesores nodo)))
    ))

; elimina los candidatos que incluyen algún ciclo en su ruta
(define elimina-ciclos 
  (lambda (nodos)
    (apply append 
           (map (lambda (nodo)
                  (let ((edo (car nodo))
                        (ruta (caddr nodo)))
                    (if (ciclo? edo (cdr ruta))
                        ()
                        (list nodo))))
                nodos))
    ))

; determina el último edo de una ruta ya pertenecía a ella
(define ciclo? 
  (lambda (edo ruta)
    (cond
      ((null? ruta) #f)
      ((equal? edo (cadar ruta)) #t)
      (else (ciclo? edo (cdr ruta))))
    ))

;-------------------- Parte Personalizada ------------------------

; Gets the r,c of the puzzle
(define (puz-ref puz r c)
 (list-ref (list-ref puz r) c))

; Devuelve una lista que contiene la posicion del 9 y la lista de movimientos posibles
(define movimientosPosibles
  ( lambda (edo)
           (cond ((equal? (puz-ref edo 0 0) 9)  (list '(0 0) (list 'right 'down)))           
                 ((equal? (puz-ref edo 0 1) 9)  (list '(0 1) (list 'left 'down 'right)))
                 ((equal? (puz-ref edo 0 2) 9)  (list '(0 2) (list 'left 'down)))
                 ((equal? (puz-ref edo 1 0) 9)  (list '(1 0) (list 'up 'down 'right)))
                 ((equal? (puz-ref edo 1 1) 9)  (list '(1 1) (list 'left 'right 'up 'down)))
                 ((equal? (puz-ref edo 1 2) 9)  (list '(1 2) (list 'left 'up 'down)))
                 ((equal? (puz-ref edo 2 0) 9)  (list '(2 0) (list 'up 'right)))
                 ((equal? (puz-ref edo 2 1) 9)  (list '(2 1) (list 'left 'up 'right)))
                 ((equal? (puz-ref edo 2 2) 9)  (list '(2 2) (list 'left 'up)))            
   )))

; Recibe un nodo de la forma (edo costo ruta), y devuelve una lista de sucesores de la forma ((op1 suc1) cto1) .... )
(define sucesores
  (lambda (nodo)
    (let* ((configTablero (movimientosPosibles (car nodo)))) 
      (map (lambda (dir) (list (list dir (slide (car nodo) (car configTablero) dir)) 1)) (cadr configTablero))
      )
    ))

; Mueve el estado board, en donde la posicion vacia es pos, en la dirección dir
(define (slide board pos dir)
    (let* ((r (car pos))
           (c (cadr pos)))
    (cond ((eq? dir 'up) (if (= r 0) null
                           (swap board r c (- r 1) c)))
          ((eq? dir 'down) (if (= r (- (length board) 1)) null
                           (swap board r c (+ r 1) c)))
          ((eq? dir 'left) (if (= c 0) null
                             (swap board r c r (- c 1))))
          ((eq? dir 'right) (if (= c (- (length (car board)) 1)) null
                              (swap board r c r (+ c 1)))))))

; Hace el switch en el tablero puz de la posicion (r1 c1) y (r2 c2)
(define (swap puz r1 c1 r2 c2)
  (let ((flatBoard (swapList puz r1 c1 r2 c2))) 
    (list 
      (list (car flatBoard) (cadr flatBoard) (caddr flatBoard)) 
      (list (cadddr flatBoard) (car (cddddr flatBoard)) (cadr (cddddr flatBoard)))
      (list (caddr (cddddr flatBoard)) (cadddr (cddddr flatBoard)) (car (cddddr (cddddr flatBoard))))
    )))

; Hace el switch de las posiciones pero devuelve el tablero en formato lista. Es decir ( 1 2 3 4 5 6 7 8 9)
(define (swapList puz r1 c1 r2 c2)
  (let* ( (temp (puz-ref puz r1 c1)) (temp2 (puz-ref puz r2 c2)))
    (map (lambda (i j) 
                        (cond ((and (= r1 i) (= c1 j)) temp2)
                              ((and (= r2 i) (= c2 j)) temp)
                                          (else (puz-ref puz i j)))
                        ) '(0 0 0 1 1 1 2 2 2) '(0 1 2 0 1 2 0 1 2))
  ))

; Estado Meta
(define meta (list (list 1 2 3) (list 8 9 4) (list 7 6 5)))

;Tablero Inicial Dificil
(define tablero (list (list 2 8 3) (list 1 6 4) (list 7 9 5)))

;Define el estado meta
(define meta?
  (lambda (nodo) 
    (cond ((equal? (car nodo) meta) #t)
          (else #f))
    ))