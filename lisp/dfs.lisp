;; Graph Search Procedure with printing
(defun graph-search (graph)
  "Performs a complete depth-first search on a graph, printing traversal order."
  (let ((visited (make-hash-table))
    (format t "~%Starting graph search~%")
    
    ;; Mark all vertices as unvisited
    (dolist (vertex (get-vertices graph))
    
    ;; Visit all connected components
    (dolist (vertex (get-vertices graph))
      (unless (gethash vertex visited)
        (format t "~%New component starting at vertex ~a~%" vertex)
        (depth-first-search vertex graph visited)))
  (format t "~%Search complete~%")))))

(defun depth-first-search (current-vertex graph visited)
  "Performs DFS starting from current-vertex, printing progress."
  (setf (gethash current-vertex visited) t)
  (format t "Visiting vertex ~a~%" current-vertex)
  
  (dolist (neighbor (get-adjacent current-vertex graph))
    (unless (gethash neighbor visited)
      (format t "  Exploring edge ~a -> ~a~%" current-vertex neighbor)
      (depth-first-search neighbor graph visited))))

;; Helper Functions (same as before)
(defun get-vertices (graph)
  "Extracts all vertices from adjacency list format graph."
  (mapcar #'car graph))

(defun get-adjacent (vertex graph)
  "Gets adjacent vertices for a given vertex."
  (cdr (assoc vertex graph)))

;; Example usage:
(setq my-graph '((1 2 3) (2 4) (3 5) (4) (5 6) (6) (7 8) (8 7)))
(graph-search my-graph)
