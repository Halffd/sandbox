(defstruct node
  value
  left
  right)

(defun pre-order-non-recursive (tree)
  (let ((stack (list (list tree 1)))  ; Start with (tree, moment 1) on stack
        (result '())
        (logs '()))
    
    (push "*** Starting pre-order traversal ***" logs)
    
    (loop while stack do
          (let* ((top (pop stack))
                 (current-node (first top))
                 (moment (second top)))
            
            (push (format nil "~%Current Stack: ~A" (format-stack stack)) logs)
            (push (format nil "Current Result: ~A" (reverse result)) logs)
            
            (cond
              ;; Moment 1: Visit node, push for moment 2, push left child
              ((= moment 1)
               (push (format nil "Moment 1: Visiting node ~A" (node-value current-node)) logs)
               (push (node-value current-node) result)
               (push (format nil "  - Adding value to result") logs)
               (push (format nil "  - Pushing same node with moment 2") logs)
               (push (list current-node 2) stack)
               
               (when (node-left current-node)
                 (push (format nil "  - Pushing left child with moment 1") logs)
                 (push (list (node-left current-node) 1) stack))
               
               (push (format nil "  - Updated stack: ~A" (format-stack stack)) logs))
              
              ;; Moment 2: Push for moment 3, push right child
              ((= moment 2)
               (push (format nil "Moment 2: Processing node ~A" (node-value current-node)) logs)
               (push (format nil "  - Pushing same node with moment 3") logs)
               (push (list current-node 3) stack)
               
               (when (node-right current-node)
                 (push (format nil "  - Pushing right child with moment 1") logs)
                 (push (list (node-right current-node) 1) stack))
               
               (push (format nil "  - Updated stack: ~A" (format-stack stack)) logs))
              
              ;; Moment 3: Nothing to do in pre-order
              ((= moment 3)
               (push (format nil "Moment 3: Finishing node ~A" (node-value current-node)) logs)
               (push (format nil "  - Nothing to do in pre-order, just removing from stack") logs)))))
    
    (push "*** Traversal complete! ***" logs)
    (values (reverse result) (reverse logs))))

(defun format-stack (stack)
  (format nil "(~{~A~^, ~})"
          (mapcar (lambda (item)
                    (format nil "(~A,~A)" 
                            (if (node-p (first item))
                                (node-value (first item))
                                "Empty")
                            (second item)))
                  stack)))

;; Create example tree from the webpage (10, 8, 2, 12, 7)
(defun make-example-tree ()
  (make-node
   :value 10
   :left (make-node
          :value 8
          :left (make-node :value 2 :left nil :right nil)
          :right (make-node :value 12 :left nil :right nil))
   :right (make-node
           :value 7
           :left nil
           :right nil)))

;; Test function
(defun run-example ()
  (multiple-value-bind (result logs)
      (pre-order-non-recursive (make-example-tree))
    (format t "~{~A~%~}" logs)
    (format t "~%Final result: ~A~%" result)))
(run-example)
