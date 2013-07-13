(in-package #:sicl-compiler-dominance)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities.

;;; This function is not actually used in the function for computing
;;; dominators.
(defun depth-first-search-preorder (start-node successor-fun)
  (let ((table (make-hash-table :test #'eq))
	(result '()))
    (labels ((traverse (node)
	       (unless (gethash node table)
		 (setf (gethash node table) t)
		 (loop for succ in (funcall successor-fun node)
		       do (traverse succ))
		 (push node result))))
      (traverse start-node))
    result))

(defun count-nodes (start-node successor-fun)
  (length (depth-first-search-preorder start-node successor-fun)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the immediate dominator of each node in an arbitrary
;;; flowgraph.
;;;
;;; This function is a very close implementation of the algorithm by
;;; Thomas Lengauer and Robert Endre Tarjan, as described in their
;;; paper "A Fast Algorithm for Finding Dominators in a Flowgraph",
;;; published in ACM Transactions on Programming Languages and
;;; Systems, Vol 1, No 1, July 1979, pages 121-141.
;;;
;;; It is close in that I even duplicated quirks such as using numbers
;;; instead of objects in some cases, and starting numbering by 1.  I
;;; also used the variable names of the paper, which makes the code a
;;; bit harder to follow by itself, but easier to verify against the
;;; paper.
;;;
;;; Although advertised as working on flowgraphs (i.e. a graph where
;;; the nodes are basic blocks), it works for any types of graph with
;;; similar characteristics, and in particular for flowchars (i.e., w
;;; graph where the nodes are individual instructions).  All that we
;;; require is that the nodes in the graph can be compared using EQ,
;;; and that a function is supplied that returns a list of successors
;;; for each node in the graph.
;;;
;;; We have tested this implementation on randomly generated graphs by
;;; comparing it to the result of a trivial algorithm for computing
;;; dominators (see below).

(defun immediate-dominators (start-node successor-fun)
  (let* ((pred (make-hash-table :test #'eq))
	 (parent (make-hash-table :test #'eq))
	 (vertex (make-array (count-nodes start-node successor-fun)))
	 (semi (make-hash-table :test #'eq))
	 (dom (make-hash-table :test #'eq))
	 (bucket (make-hash-table :test #'eq))
	 (label (make-hash-table :test #'eq))
	 (ancestor (make-hash-table :test #'eq))
	 (n 0))
    (flet ((pred (node)
	     (gethash node pred))
	   ((setf pred) (new-pred node)
	     (setf (gethash node pred) new-pred))
	   (parent (node)
	     (gethash node parent))
	   ((setf parent) (new-parent node)
	     (setf (gethash node parent) new-parent))
	   (vertex (i)
	     (aref vertex (1- i)))
	   ((setf vertex) (new-vertex i)
	     (setf (aref vertex (1- i)) new-vertex))
	   (semi-default (node)
	     (gethash node semi 0))
	   (semi (node)
	     (multiple-value-bind (value present-p)
		 (gethash node semi)
	       (assert present-p)
	       value))
	   ((setf semi) (new-semi node)
	     (setf (gethash node semi) new-semi))
	   (dom (node)
	     (gethash node dom))
	   ((setf dom) (new-dom node)
	     (setf (gethash node dom) new-dom))
	   (bucket (node)
	     (gethash node bucket))
	   ((setf bucket) (new-bucket node)
	     (setf (gethash node bucket) new-bucket))
	   (label (node)
	     (multiple-value-bind (value present-p)
		 (gethash node label)
	       (assert present-p)
	       value))
	   ((setf label) (new-label node)
	     (setf (gethash node label) new-label))
	   (ancestor (node)
	     (multiple-value-bind (value present-p)
		 (gethash node ancestor)
	       (assert present-p)
	       value))
	   ((setf ancestor) (new-ancestor node)
	     (setf (gethash node ancestor) new-ancestor)))
      ;; Step 1
      (labels ((dfs (v)
		 (setf (semi v) (incf n))
		 (setf (vertex n) v)
		 (loop for w in (successors v)
		       do (when (zerop (semi-default w))
			    (setf (parent w) v)
			    (dfs w))
			  (push v (pred w)))))
	(dfs start-node))
      (labels ((evaluate (v)
		 (if (null (ancestor v))
		     v
		     (progn (compress v)
			    (label v))))
	       (compress (v)
		 (unless (null (ancestor (ancestor v)))
		   (compress (ancestor v))
		   (when (< (semi (label (ancestor v))) (semi (label v)))
		     (setf (label v) (label (ancestor v))))
		   (setf (ancestor v) (ancestor (ancestor v)))))
	       (link (v w)
		 (setf (ancestor w) v)))
	;; Step 2 and 3
	(loop for i from 1 to n
	      for v = (vertex i)
	      do (setf (ancestor v) nil)
		 (setf (label v) v))
	(loop for i downfrom n to 2
	      for w = (vertex i)
	      do (loop for v in (pred w)
		       for u = (evaluate v)
		       do (when (< (semi u) (semi w))
			    (setf (semi w) (semi u)))
			  (push w (bucket (vertex (semi w))))
			  (link (parent w) w))
		 (loop for v in (bucket (parent w))
		       do (setf (bucket (parent w))
				(remove v (bucket (parent w)) :test #'eq))
			  (let ((u (evaluate v)))
			    (setf (dom v)
				  (if (< (semi u) (semi v))
				      u
				      (parent w))))))
	(loop for i from 2 to n
	      for w = (vertex i)
	      do (unless (eq (dom w) (vertex (semi w)))
		   (setf (dom w) (dom (dom w))))))
      dom)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the complete dominance tree.  
;;;
;;; We represent the complete dominance tree as a hash table with the
;;; nodes of the flowgraph as keys.  The value for a particular node
;;; is a list starting with the node itself, then the immediate
;;; dominator of the node, then the immediate dominator of the
;;; immediate dominator of the node, etc.  The last element of the
;;; list is always the root node of the flowgraph.
;;;
;;; By representing it this way, we can use the dominance tree to
;;; return all insteresting dominance relations.  By itself, a value
;;; in the table is a set of all dominators of a node.  The CADR of
;;; that list is the IMMEDIATE dominator of a node, The CDR of the
;;; list is the set of all STRICT dominators of the node. 

(defun dominance-tree (start-node successor-fun)
  (let ((immediate-dominators (immediate-dominators start-node successor-fun))
	(dominators (make-hash-table :test #'eq)))
    (setf (gethash start-node dominators) (list start-node))
    (labels ((find-dominator-tree (node)
	       (or (gethash node dominators)
		   (setf (gethash node dominators)
			 (cons node (find-dominator-tree
				     (gethash node immediate-dominators)))))))
      (loop for node being each hash-key of immediate-dominators
	    do (find-dominator-tree node)))
    dominators))

(defun dominators (dominance-tree node)
  (gethash node dominance-tree))

(defun immediate-dominator (dominance-tree node)
  (cadr (gethash node dominance-tree)))

(defun strict-dominators (dominance-tree node)
  (cdr (gethash node dominance-tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dominance frontiers.

(defun children (dominance-tree)
  (let ((result (make-hash-table :test #'eq)))
    (loop for node being each hash-key of dominance-tree
	  do (let ((idom (immediate-dominator dominance-tree node)))
	       (unless (null idom)
		 (push node (gethash idom result)))))
    result))
	       
(defun dominance-frontiers (start-node successor-fun)
  (let* ((dominance-tree (dominance-tree start-node successor-fun))
	 (children (children dominance-tree))
	 (result (make-hash-table :test #'eq)))
    (flet ((children (node)
	     (gethash node children))
	   (successors (node)
	     (funcall successor-fun node))
	   (df (node)
	     (gethash node result))
	   ((setf df) (new-df node)
	     (setf (gethash node result) new-df)))
      (labels ((traverse (node)
		 (mapc #'traverse (children node))
		 (loop for succ in (successors node)
		       do (unless (eq (immediate-dominator dominance-tree succ)
				      node)
			    (pushnew succ (df node) :test #'eq)))))
	(traverse start-node)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test

(defclass node ()
  ((%name :initform (gensym) :reader name)
   (%successors :initform '() :accessor successors)))

;;; The probability of giving a node in layer 0 zero successors.
(defparameter *initial-zero-successor-probability* 0.05)

;;; For each layer, the probability of giving a node zero successors
;;; is modified as follows: P(i+1) = 1 - ZSNF*(1 - P(i)), where ZSNF
;;; is the value of this variable.  The closer this value is to 1, the
;;; more layers the graph will have. 
(defparameter *zero-successor-narrowing-factor* 0.99)

(defun new-zero-successor-probability (zero-successor-probability )
  (- 1.0 (* *zero-successor-narrowing-factor*
	    (- 1.0 zero-successor-probability))))

;;; The probability of a node with at least one successor to have two
;;; successors.
(defparameter *two-successor-probability* 0.4)

;;; The probability of a successor being in a lower layer.
(defparameter *back-edge-probability* 0.2)

(defun random-flow-chart ()
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
		   (if (< r2 *back-edge-probability*)
		       ;; The first successor is a back edge.
		       (set-random-back-arc node)
		       ;; Not a back edge.  Create a new node.
		       (push (new-node) (successors node)))
		   (when (< r3 *two-successor-probability*)
		     ;; The node has a second successor
		     (if (< r4 *back-edge-probability*)
			 ;; The second successor is a back edge.
			 (set-random-back-arc node)
			 ;; Not a back edge.  Create a new node.
			 (push (new-node) (successors node))))))))
	(loop for layer-number from 0
	      for zsp = *initial-zero-successor-probability*
		then (new-zero-successor-probability zsp)
	      until (null layer)
	      do (loop for node in layer
		       do (process-node node zsp))
		 (setf layer next-layer)
		 (setf next-layer '()))))
    initial))
			
(defun draw-flow-chart (initial-node filename)
  (with-open-file (stream filename
			  :direction :output
			  :if-exists :supersede)
    (format stream "digraph G {~%")
    (let ((table (make-hash-table :test #'eq)))
      (labels ((draw-node (node)
		 (unless (gethash node table)
		   (setf (gethash node table) t)
		   (loop for succ in (successors node)
			 do (draw-node succ))
		   (format stream
			   "   ~a [label = \"~a\"];~%"
			   (name node) (name node))
		   (loop for succ in (successors node)
			 do (format stream
				    "   ~a -> ~a;~%"
				    (name node)
				    (name succ))))))
	(draw-node initial-node)))
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

(defun draw-dominance-tree (dominance-tree filename)
  (with-open-file (stream filename
			  :direction :output
			  :if-exists :supersede)
    (format stream "digraph G {~%   ordering = in;~%")
    ;; Draw all the nodes first.
    (loop for node being each hash-key of dominance-tree
	  do (format stream "   ~a [label = \"~a\"];~%"
		     (name node) (name node)))
    ;; Now draw all the arcs.
    (loop for node being each hash-key of dominance-tree
	  do (let ((idom (immediate-dominator dominance-tree node)))
	       (unless (null idom)
		 (format stream "   ~a -> ~a;~%"
			 (name node) (name idom)))))
    (format stream "}~%")))

(defun draw-dominance-frontiers (start-node successor-fun filename)
  (let ((dominance-frontiers (dominance-frontiers start-node successor-fun))
	(table (make-hash-table :test #'eq)))
    (with-open-file (stream filename
			    :direction :output
			    :if-exists :supersede)
      (format stream "digraph G {~% ordering = out;~%")
      (labels ((draw-node (node)
		 (unless (gethash node table)
		   (setf (gethash node table) t)
		   (loop for succ in (successors node)
			 do (draw-node succ))
		   (format stream
			   "   ~a [label = \"~a\"];~%"
			   (name node) (name node))
		   (loop for succ in (successors node)
			 do (format stream
				    "   ~a -> ~a;~%"
				    (name node)
				    (name succ))))))
	(draw-node start-node))
      (loop for node being each hash-key of dominance-frontiers
	    do (let ((f (gethash node dominance-frontiers)))
		 (loop for ff in f
		       do (format stream
				  "   ~a -> ~a [style = bold, color = red];"
				  (name node) (name ff)))))
      (format stream "}~%"))))
    
;;; This algorithm applies the definition of dominators by examining
;;; all possible paths from the root of the flow chart to a node N,
;;; and removing any node not on that path from the possible
;;; dominators of N.
(defun slow-dominators (start-node successor-fun)
  (let* ((preorder (depth-first-search-preorder start-node successor-fun))
	 (dominators (let ((table (make-hash-table :test #'eq)))
		       (loop for node in preorder
			     do (setf (gethash node table) preorder))
		       table)))
    (flet ((successors (node)
	     (funcall successor-fun node))
	   (dominators (node)
	     (gethash node dominators))
	   ((setf dominators) (new-dominators node)
	     (setf (gethash node dominators) new-dominators)))
      (loop for node in preorder
	    do (labels ((traverse (n path)
			  (cond ((eq n node)
				 (setf (dominators node)
				       (intersection (dominators node)
						     (cons n path)
						     :test #'eq)))
				((member n path :test #'eq)
				 nil)
				(t
				 (loop for succ in (successors n)
				       do (traverse succ (cons n path)))))))
		 (traverse start-node '()))))
    dominators))
						       
						     
(defun test-one-chart (start-node)
  (let ((d1 (dominance-tree start-node #'successors))
	(d2 (slow-dominators start-node #'successors))
	(preorder (depth-first-search-preorder start-node #'successors)))
    (loop for node in preorder
	  do (unless (and (null (set-difference (gethash node d1)
						(gethash node d2)))
			  (null (set-difference (gethash node d2)
						(gethash node d1))))
	       (return (values node d1 d2))))))

(defun test-dominators (n)
  (loop repeat n
	do (test-one-chart (random-flow-chart))))
