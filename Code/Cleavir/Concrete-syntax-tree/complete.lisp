(cl:in-package #:cleavir-cst)

;;;; By COMPLETING a CST we mean computing the expression that it
;;;; represents and associating each object of that expression by the
;;;; corresponding node in the CST.
;;;;
;;;; The input is a CST such that the leaves have Common Lisp atoms in
;;;; the EXPRESSION slot, but the value of the EXPRESSION slot of all
;;;; other nodes in the CST might not be valid.

(defun complete (cst)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((aux (cst)
	       (unless (gethash cst table)
		 (setf (gethash cst table) t)
		 (multiple-value-bind (cell-count structure)
		     (cleavir-code-utilities:list-structure (children cst))
		   (ecase structure
		     (:proper
		      (setf (expression cst)
			    (loop for child in (children cst)
				  do (aux child)
				  collect (expression child))))
		     (:dotted
		      (if (zerop cell-count)
			  ;; This CST represents a Common Lisp atom,
			  ;; so it is already complete.
			  nil
			  (setf (expression cst)
				(loop for rest = (children cst) then (cdr rest)
				      until (atom rest)
				      if (atom (cdr rest))
					append
					(progn (aux (car rest))
					       (aux (cdr rest))
					       (cons (expression (car rest))
						     (expression (cdr rest))))
				      else
					collect
					(progn (aux (car rest))
					       (expression (car rest)))))))
		     (:circular
		      (let ((temp (loop with children = (children cst)
					for rest = children then (cdr rest)
					repeat cell-count
					collect
					(progn (aux (car rest))
					       (expression (car rest))))))
			(setf (cdr (last temp)) temp)
			(setf (expression cst) temp))))))))
      (aux cst))))
					
