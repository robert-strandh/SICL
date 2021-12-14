(cl:in-package #:cleavir-test-utilities)

;;;; This file contains code for generating a random flowchart and for
;;;; drawing such a chart as a Graphviz file.

(defclass node ()
  ((%name :initform (gensym) :reader name)
   (%inputs :initform '() :accessor inputs)
   (%outputs :initform '() :accessor outputs)
   (%successors :initform '() :accessor successors)))

(defun make-node (&optional successors)
  (make-instance 'node :successors successors))

(defmethod print-object ((object node) stream)
  (print-unreadable-object (object stream)
    (format stream "~a" (name object))))

;;; The probability of giving a node in layer 0 zero successors.
(defparameter *initial-zero-successor-probability* 0.05)

;;; For each layer, the probability of giving a node zero successors
;;; is modified as follows: P(i+1) = 1 - ZSNF*(1 - P(i)), where ZSNF
;;; is the value of this variable.  The closer this value is to 1, the
;;; more layers the graph will have. 
(defparameter *zero-successor-narrowing-factor* 0.99)

(defun new-zero-successor-probability
    (zero-successor-probability zero-successor-narrowing-factor)
  (- 1.0 (* zero-successor-narrowing-factor
            (- 1.0 zero-successor-probability))))

;;; The probability of a node with at least one successor to have two
;;; successors.
(defparameter *two-successor-probability* 0.4)

;;; The probability of a successor being in a lower layer.
(defparameter *back-edge-probability* 0.2)

(defun random-inputs (vars)
  (let ((r (random 1.0)))
    (cond ((< r 0.05)
           '())
          ((< r 0.20)
           (list (elt vars (random (length vars)))))
          (t
           (let* ((v1 (elt vars (random (length vars))))
                  (v2 (elt (remove v1 vars) (random (1- (length vars))))))
             (list v1 v2))))))

(defun random-outputs (vars)
  (let ((r (random 1.0)))
    (cond ((< r 0.90)
           (list (elt vars (random (length vars)))))
          ((< r 0.95)
           '())
          (t
           (let* ((v1 (elt vars (random (length vars))))
                  (v2 (elt (remove v1 vars) (random (1- (length vars))))))
             (list v1 v2))))))

(defun assign-random-inputs/outputs (start-node)
  (let* ((node-count (cleavir-utilities:count-nodes
                      start-node #'successors))
         (variables (loop repeat (+ (round (* node-count 1)) 2)
                          collect (gensym))))
    (cleavir-utilities:map-nodes
     start-node #'successors
     (lambda (node)
       (setf (inputs node) (random-inputs variables))
       (setf (outputs node) (random-outputs variables))))))

(defun random-flow-chart
    (&optional
       (izsp *initial-zero-successor-probability*)
       (zsnf *zero-successor-narrowing-factor*)
       (tsp *two-successor-probability*)
       (bep *back-edge-probability*))
  (let* ((initial (make-instance 'node))
         (layer (list initial))
         (next-layer '())
         (all-nodes (list initial)))
    (flet ((new-node ()
             (let ((node (make-instance 'node)))
               (push node all-nodes)
               (push node next-layer)
               node))
           (set-random-back-arc (node)
             (let ((all (set-difference all-nodes (successors node)
                                        :test #'eq)))
               (unless (null all)
                 (push (elt all (random (length all)))
                       (successors node))))))
      (flet ((process-node (node zsp)
               (let ((r1 (random 1.0))
                     (r2 (random 1.0))
                     (r3 (random 1.0))
                     (r4 (random 1.0)))
                 (unless (< r1 zsp)
                   ;; The node has at least one successor.
                   (if (< r2 bep)
                       ;; The first successor is a back edge.
                       (set-random-back-arc node)
                       ;; Not a back edge.  Create a new node.
                       (push (new-node) (successors node)))
                   (when (< r3 tsp)
                     ;; The node has a second successor
                     (if (< r4 bep)
                         ;; The second successor is a back edge.
                         (set-random-back-arc node)
                         ;; Not a back edge.  Create a new node.
                         (push (new-node) (successors node))))))))
        (loop for layer-number from 0
              for zsp = izsp
                then (new-zero-successor-probability zsp zsnf)
              until (null layer)
              do (loop for node in layer
                       do (process-node node zsp))
                 (setf layer next-layer)
                 (setf next-layer '()))))
    (assign-random-inputs/outputs initial)
    (values initial #'successors #'inputs #'outputs)))

(defun draw-flow-chart (initial-node filename)
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede)
    (format stream "digraph G {~%")
    ;; First draw all the nodes.
    (cleavir-utilities:map-nodes
     initial-node #'successors
     (lambda (node)
       (format stream
               "   ~a [shape = box, label = \"~a\"];~%"
               (name node) (name node))))
    ;; Next draw all the links between nodes
    (cleavir-utilities:map-nodes
     initial-node #'successors
     (lambda (node)
       (loop for succ in (successors node)
             do (format stream
                        "   ~a -> ~a [style = bold];~%"
                        (name node)
                        (name succ)))))
    
     ;; Draw all the variables.
     (let ((table (make-hash-table :test #'eq)))
       (cleavir-utilities:map-nodes
        initial-node #'successors
        (lambda (node)
          (loop for var in (append (inputs node) (outputs node))
                do (unless (gethash var table)
                     (setf (gethash var table) t)
                     (format stream
                             "   ~a [shape = ellipse, label = \"~a\"];~%"
                             var var))))))
    ;; Draw all the inputs and outputs.
    (cleavir-utilities:map-nodes
     initial-node #'successors
     (lambda (node)
       (loop for var in (inputs node)
             do (format stream
                        "   ~a -> ~a [color = red, style = dashed];~%"
                        var (name node)))
       (loop for var in (outputs node)
             do (format stream
                        "   ~a -> ~a [color = blue, style = dashed];~%"
                        (name node) var))))
    (format stream "}~%")))

(defun draw-preorder (preorder filename)
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede)
    (format stream "digraph G {~% ordering = out;~%")
    ;; Draw all the nodes first.
    (loop for node in preorder
          do (format stream
                     "   ~a [label = \"~a\"];~%"
                     (name node) (name node)))
    ;; Now draw the arcs
    (loop for node in preorder
          do (loop for succ in (successors node)
                   do (format stream
                              (if (<= (position succ preorder)
                                      (position node preorder))
                                  "   ~a -> ~a [style = dashed];~%"
                                  "   ~a -> ~a;~%")
                              (name node)
                              (name succ))))
    (format stream "}~%")))
