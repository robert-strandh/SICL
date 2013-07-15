(in-package #:sicl-compiler-reaching-definitions)

(defun same-set-p (set1 set2)
  (and (subsetp set1 set2 :test #'eq)
       (subsetp set2 set1 :test #'eq)))

(defun b-to-a (node btable atable definitions)
  (let* ((before (gethash node btable))
	 (stripped (remove-if (lambda (x)
				(member (cdr x) definitions
					:test #'eq :key #'cdr))
			      before)))
    (setf (gethash node atable)
	  (append definitions stripped))))

;;; A DEFINITION is a CONS of a NODE and some VARIABLE.  This function
;;; can handle any concept of NODE and VARIABLE as long as they can be
;;; compared with EQ.  
;;;
;;; Return a hash table.  A key in the hash table is a node and a
;;; value is a list of definitions that reach that node.
(defun reaching-definitions (start-node successor-fun out-fun)
  (let ((predecessor-fun (sicl-compiler-utilities:predecessor-function
			   start-node successor-fun)))
    (flet ((successors (node)
	     (funcall successor-fun node))
	   (predecessors (node)
	     (funcall predecessor-fun node))
	   (outputs (node)
	     (funcall out-fun node)))
      (let ((definitions (make-hash-table :test #'eq)))
	;; Collect all definitions.  We create a definition once, so
	;; that even though it is a CONS cell, we can compare two
	;; defintions using EQ.
	(sicl-compiler-utilities:map-nodes
	 start-node successor-fun
	 (lambda (node)
	   (loop for out in (outputs node)
		 do (push (cons node out)
			  (gethash node definitions)))))
	(flet ((definitions (node)
		 (gethash node definitions)))
	  (let ((btable (make-hash-table :test #'eq))
		(atable (make-hash-table :test #'eq)))
	    (labels ((traverse (node)
		       (multiple-value-bind (value present-p)
			   (gethash node btable)
			 (let* ((new (apply
				      #'concatenate 'list
				      (mapcar (lambda (n) (gethash n atable))
					      (predecessors node))))
				(unique (remove-duplicates new :test #'eq)))
			   (when (or (not present-p)
				     (not (same-set-p value unique)))
			     (setf (gethash node btable) unique)
			     (b-to-a node btable atable (definitions node))
			     (mapc #'traverse (successors node)))))))
	      (traverse start-node))
	    btable))))))
