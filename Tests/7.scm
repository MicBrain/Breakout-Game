; Author: Loïc Faure-Lacroix
(require "extras")
(require "srfi-69")
(require "srfi-1")

(require "disjoint-set")
(require "list-util")

; Check of tree of a contains b
; Check if both tree roots are the same
(define (contains forest a b)
  (let ((root (ds-find forest a))
        (node (ds-find forest b)))
    (equal? root node)))

; Get first edge in the list of edges that has
; at least one node in the tree but not both
; it returns the first match
(define (get-edge forest root edges)
  (find (lambda (edge)
    (let ((nodeA (list-ref edge 1))
          (nodeB (list-ref edge 2)))
      (and (not (contains forest nodeA nodeB))
           (or (contains forest root nodeA)
               (contains forest root nodeB)))))
    edges))

; Prim algorithm we have to copy
(define (prim nodes edges)
  (define (prim-forest forest result root edges)
    (let ((edge (get-edge forest root edges)))
      ; Get first possible edge in the edge list
      ; if there is no edge, it means we might have added
      ; all edges to the tree.
      ; edges aren't removed from the list might get slow with big
      ; graphs but should be good enough for small graphs since
      ; we don't allocate/deallocate memory
      (if edge
        (begin
            ; if edge is found we join it to our forest using the root
            ; as root
            (ds-union forest root (list-ref edge 1))
            (ds-union forest root (list-ref edge 2))
            ; Recurse for new edges and append the current edge to the results
            (prim-forest forest (append result (list edge)) root edges))
        ; Otherwise we return the result
        result)))

  ; Code starts here
  ; Sort our edges by increasing weights
  (sort-nodes edges)
  (let ((forest (make-hash-table)) (mst (list)) (root (car nodes)))
    ; Fill our forest
    (fill-forest forest nodes)
    ; Find our results
    (prim-forest forest mst root edges)))