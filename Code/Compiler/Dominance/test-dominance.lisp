(cl:in-package #:sicl-compiler-dominance-test)

(defun depth-first-search-preorder (start-node successor-fun)
  (sicl-compiler-utilities:depth-first-search-preorder
   start-node successor-fun))

(defun name (node)
  (sicl-compiler-test-utilities:name node))

(defun successors (node)
  (sicl-compiler-test-utilities:successors node))

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
  (let ((d1 (sicl-compiler-dominance:dominance-tree start-node #'successors))
	(d2 (slow-dominators start-node #'successors))
	(preorder (depth-first-search-preorder start-node #'successors)))
    (loop for node in preorder
	  do (unless (and (null (set-difference (gethash node d1)
						(gethash node d2)))
			  (null (set-difference (gethash node d2)
						(gethash node d1))))
	       (return (values node d1 d2))))))

(defun test-dominance (&optional (n 10000))
  (loop repeat n
	do (let ((f (sicl-compiler-test-utilities:random-flow-chart)))
	     (when (< (sicl-compiler-utilities:count-nodes
		       f #'sicl-compiler-test-utilities:successors)
		      50)
	       (test-one-chart f)))))

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
	  do (let ((idom (sicl-compiler-dominance:immediate-dominator
			  dominance-tree node)))
	       (unless (null idom)
		 (format stream "   ~a -> ~a;~%"
			 (name node) (name idom)))))
    (format stream "}~%")))

(defun draw-dominance-frontiers (start-node successor-fun filename)
  (let ((dominance-frontiers (sicl-compiler-dominance:dominance-frontiers
			      start-node successor-fun))
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
    
