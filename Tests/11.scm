;;; Joe Wright - August 2013

; Non-standard primitives added:
;   sleep
;   random
;   square-root
;   sin, cos, tan
;   asin, acos, atan
;   screen-size

;;; Variables
; Number of boids on screen
(define num-boids 30)
; Animation settings
(define frame-interval (/ 1 24)) ; ideally 24 fps

;;; Prepare boids - data abstraction
; Constructors. Boids have a position and velocity, represented as 2D vectors
(define (mkvec x y) (cons x y))

(define (make-boid xpos ypos xvel yvel)
  (begin 
     (define position (mkvec xpos ypos)) ; Initialize at origin
     (define velocity (mkvec
                        (- 0.5 (* (random) 1))    ; Initialize with
                        (- 0.5 (* (random) 1)) )) ; random velocity (x, y)
     (lambda (m) 
        (cond 
            ((eq? m 'pos) position)
            ((eq? m 'vel) velocity)
         )
        )
    )
 )

(define (make-rand-boid)
  (let
    ((xpos (- (* 0.5 (car (screensize))) (* (random) (car (screensize)) ))) 
     (ypos (- (* 0.5 (cdr (screensize))) (* (random) (cdr (screensize)) ))) 
     (xvel (- 5 (* (random) 10)) ) 
     (yvel (- 5 (* (random) 10))) )
    (make-boid xpos ypos xvel yvel)
    )
 )
; Selectors
(define (vecx vec) (car vec))
(define (vecy vec) (cdr vec))
(define (vecmag vec) (square-root (+ (* (vecx vec) (vecx vec) ) (* (vecy vec) (vecy vec)) )))

(define (boid-pos boid) (boid 'pos))
(define (boid-vel boid) (boid 'vel))
(define (boid-xpos boid) (vecx (boid 'pos)) )
(define (boid-ypos boid) (vecy (boid 'pos)) )
(define (boid-xvel boid) (vecx (boid 'vel)) )
(define (boid-yvel boid) (vecy (boid 'vel)) )

; Boid characterstics
(define (vecadd vec1 vec2) (mkvec 
                             (+ (vecx vec1) (vecx vec2))
                             (+ (vecy vec1) (vecy vec2))
                             ))
(define (vecscale vec k) (mkvec 
                            (* k (vecx vec))
                            (* k (vecy vec))
                            ))
(define (vecsub vec1 vec2) (mkvec 
                             (- (vecx vec1) (vecx vec2))
                             (- (vecy vec1) (vecy vec2))
                             )
  )
(define (boid-speed boid) (vecmag (boid 'vel)))
(define (boid-distance boid) (vecmag (boid 'pos)))

(define (boid-heading boid) (begin (define angle (atan (/ 
                                                         (boid-yvel boid) 
                                                         (boid-xvel boid) 
                                                         )) )
                                   (cond
                                     ((> 0 (boid-xvel boid)) (- 270 angle))
                                     ((< 0 (boid-xvel boid)) (- 90 angle) )
                                     (else (if (> 0 (boid-yvel boid)) 0 180) )
                                     )))
(define (boid-distance boid1 boid2)
  (square-root
    (+
      (* 
        (- (boid-xpos boid1) (boid-xpos boid2))
        (- (boid-xpos boid1) (boid-xpos boid2))
        )
      (*
        (- (boid-ypos boid1) (boid-ypos boid2))
        (- (boid-ypos boid1) (boid-ypos boid2))
        ))))

; Create all boids, stored in a list
(define (make-boid-list n)


  (define (add-boid lst n)
    (if (< 0 n) (add-boid (append lst (list(make-rand-boid))) (- n 1)) lst) 
    )
  (add-boid nil n)
  )

(define all-boids (make-boid-list num-boids))

;;; Calculate boid position updates

; Separation - distance from neighbors
(define max-distance 15)
(define (separation boid boids)
  ; Calculate how much to add to vector
  (define (calc-sep other-boid) 
    (if (eq? (boid-pos boid) (boid-pos other-boid)) ; same boid
      (mkvec 0 0)
      (if (> (boid-distance boid other-boid) max-distance) (mkvec 0 0) 
        (vecsub (boid-pos other-boid) (boid-pos boid))
        )
      )
    )
  ; Iterate through all boids
  (if (null? boids) (mkvec 0 0)
    (vecadd (calc-sep (car boids)) (separation boid (cdr boids)) )
    )
  
  )

; Alignment - going same direction as neighbors
(define (avg-vel boids) ; find total average velocity, scaled down by 1/10
  (if (null? boids) (mkvec 0 0) 
    (vecadd (boid-vel (car boids)) (avg-vel (cdr boids)))
  )
)
(define (alignment boid boids)
  (vecscale (vecsub (vecscale (avg-vel boids) (/ 1 num-boids)) (boid-vel boid)) (/ 1 10))
  )

; Cohesion - Stick with group
(define (avg-pos boids) ; find total average position, scaled down by 1/100
  (if (null? boids) (mkvec 0 0) 
    (vecadd (boid-pos (car boids)) (avg-pos (cdr boids)))
  )
)
(define (cohesion boid boids) 
  (vecscale (vecsub (vecscale (avg-vel boids) (/ 1 num-boids)) (boid-vel boid)) (/ 1 100))
  )

; Bounding - don't run into edges
(define (bounding boid boids)
    (mkvec 0 0)
  )

(define (update-boid boid boids)
  (define new-vel (vecadd (boid-vel boid) (vecadd 
                                            (alignment boid boids) 
                                            (vecadd 
                                              (cohesion boid boids) 
                                              (bounding boid boids))) ))
  (define new-pos (vecadd (boid-pos boid) (vecadd (boid-vel boid) new-vel)))
  (make-boid (vecx new-pos) (vecy new-pos) (vecx new-vel) (vecy new-vel))
  )

(define (update-boids boids all-boids)
  (define (helper boids lst)
    (if (null? boids) lst
      (helper (cdr boids)
        (append lst (list (update-boid (car boids) all-boids)))
       ))
    )
  (helper boids nil)
  )

;;; Draw boids

(define (draw-boid boid)
  (begin
    (define radius 4) ; use to control triange size
    (penup)
    (setposition (boid-xpos boid) (boid-ypos boid))
    (setheading (boid-heading boid))
    (forward (* 2 radius))
    (right 14.04)
    (pendown)
    (forward (square-root (+ 
                            (* (* 4 radius) (* 4 radius))
                            (* radius radius )
                            )))
    (right 104.04) ; 75.96
    (forward (* 2 radius))
    (right 104.04)
    (forward (square-root (+ 
                            (* (* 4 radius) (* 4 radius))
                            (* radius radius )
                            )))
 
    )

  )

(define (draw-boids boids)
  (if (null? boids) 'okay
    (begin
      (draw-boid (car boids))
      (draw-boids (cdr boids))
      )
  
    )
 )
;;; Execute program
(speed 0)
(delay 0)
(tracer 6)
(ht)

(define (go) (begin 
               (clear) 
               (define all-boids (update-boids all-boids all-boids)) 
               (draw-boids all-boids)
               (save-image)
               (sleep frame-interval)
               (go)
               ))