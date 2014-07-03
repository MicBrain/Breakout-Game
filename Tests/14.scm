use srfi-1) ; chicken-scheme-specific import


;; MLP

(define (logistic-function x)
  (/ 1 (+ 1 (exp (- x)))))
(define (logistic-function-derivative x)
  (* (logistic-function x) (- 1 (logistic-function x))))
(define (tanh x)
  (cond ((< x -20) -1.0)
        ((> x 20) 1.0)
        (else (let ((y (exp (- (* 2 x)))))
                (/ (- 1 y) (+ 1 y))))))
(define (tanh-derivative x)
  (cond ((or (< x -20)
            (> x 20)) 0)
        (else (- 1 (expt (tanh x) 2)))))


(define (propagate-through-neurons inputs af)
  (map af inputs))

(define (propagate-through-weights neuron-outputs weights)
  (map (lambda (output weights)
         (map (lambda (weight) (* weight output))
              weights))
       neuron-outputs
       weights))

(define (prepare-inputs raw-inputs)
  (fold (lambda (input result)
          (map + input result))
        (make-list (length (car raw-inputs)) 0)
        raw-inputs))

(define (propagate weights inputs af)
  (if (null? weights)
      '()
      (let* ((neuron-outputs (propagate-through-neurons inputs af))
             (next-raw-inputs (propagate-through-weights neuron-outputs (car weights)))
             (next-inputs (prepare-inputs next-raw-inputs)))
        (cons (list inputs neuron-outputs next-raw-inputs next-inputs)
              (propagate (cdr weights) next-inputs af)))))


;; backpropagation
;; http://www.cs.bham.ac.uk/~jxb/NN/l7.pdf

(define (propagate-through-weights-backwards deltas weights)
  (map (lambda (weight-set)
         (map (lambda (delta weight)
                (* weight delta))
              deltas
              weight-set))
       weights))

(define (backpropagate-further previous-deltas weights propagation-results afd)
  (if (null? (cdr weights))
      previous-deltas
      (let* ((propagation-result (car propagation-results))
             (propagation-result-neuron-inputs (car propagation-result))
             (propagation-result-neuron-outputs (cadr propagation-result))
             (propagation-result-next-inputs (cadddr propagation-result))
             (next-layer-delta (car previous-deltas))
             (weights-between-current-and-next-layer (car weights))
             (weighted-sum-for-delta
              (map (lambda (x) (apply + x))
                   (propagate-through-weights-backwards next-layer-delta
                                                        weights-between-current-and-next-layer)))
             (current-delta (map (lambda (ws in)
                                   (* ws (afd in)))
                                 weighted-sum-for-delta
                                 propagation-result-neuron-inputs)))
        (backpropagate-further (cons current-delta previous-deltas)
                               (cdr weights)
                               (cdr propagation-results)
                               afd))))

(define (calculate-weight-deltas deltas propagation-result learning-rate)
  (map (lambda (layer-deltas layer-propagation-result)
         (let ((layer-neurons-output (cadr layer-propagation-result)))
           (map (lambda (out)
                  (map (lambda (delta)
                         (* learning-rate
                            delta
                            out))
                       layer-deltas))
                layer-neurons-output)))
       deltas
       propagation-result))

(define (backpropagate weights inputs target-outputs af afd learning-rate)
  (let* ((propagation-result (propagate weights inputs af))
         (reverse-propagation-result (reverse propagation-result))
         (last-hidden-layer-propagation-result (car reverse-propagation-result))
         (output-layer-inputs (cadddr last-hidden-layer-propagation-result))
         (output-layer-outputs (propagate-through-neurons output-layer-inputs af))
         (output-layer-delta (map (lambda (targ out in)
                                    (* (- targ out)
                                       (afd in)))
                                  target-outputs
                                  output-layer-outputs
                                  output-layer-inputs))
         (all-deltas (backpropagate-further (list output-layer-delta)
                                            (reverse weights)
                                            reverse-propagation-result
                                            afd))
         (weight-deltas (calculate-weight-deltas all-deltas propagation-result learning-rate))
         (new-weights (map (lambda (weight-layer delta-layer)
                             (map (lambda (w d)
                                    (map + w d))
                                  weight-layer
                                  delta-layer))
                           weights
                           weight-deltas)))
    new-weights))


;; helper functions

(define (propagate-final weights inputs af)
  (let* ((propagation-result (propagate weights inputs af))
         (reverse-propagation-result (reverse propagation-result))
         (last-hidden-layer-propagation-result (car reverse-propagation-result))
         (output-layer-inputs (cadddr last-hidden-layer-propagation-result))
         (output-layer-outputs (propagate-through-neurons output-layer-inputs af)))
    output-layer-outputs))

(define (train-list weights inputs outputs af afd learning-rate)
  (if (null? inputs)
      weights
      (train-list (backpropagate weights (car inputs) (car outputs) af afd learning-rate)
                  (cdr inputs)
                  (cdr outputs)
                  af
                  afd
                  learning-rate)))

(define (train weights inputs outputs af afd learning-rate n)
  (if (<= n 0)
      weights
      (train (train-list weights inputs outputs af afd learning-rate)
             inputs
             outputs
             af
             afd
             learning-rate
             (- n 1))))

(define (init-weights layers)
  (if (or (null? layers)
          (null? (cdr layers)))
      '()
      (cons (make-list* (car layers)
                        (lambda () (make-list* (cadr layers)
                                          (lambda () (- (/ (random 420)
                                                      (+ 420 (random 420)))
                                                   0.5))
                                          '()))
                        '())
            (init-weights (cdr layers)))))

(define (make-list* n thunk result)
  (if (> n 0)
      (make-list* (- n 1)
                  thunk
                  (cons (thunk) result))
      result))


;; test

(define init (init-weights '(2 3 1)))

(define trained-xor (train init
                           '((0 0) (0 1) (1 0) (1 1))
                           '((0) (1) (1) (0))
                           tanh
                           tanh-derivative
                           0.2
                           400))

(propagate-final trained-xor
                 '(0 0)
                 tanh)
(propagate-final trained-xor
                 '(0 1)
                 tanh)
(propagate-final trained-xor
                 '(1 0)
                 tanh)
(propagate-final trained-xor
                 '(1 1)
                 tanh)