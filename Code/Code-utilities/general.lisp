(in-package #:sicl-code-utilities)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tools for checking for proper lists

;;; For any object, return its structure as a list as two values: the
;;; first value contains the number of unique CONS cells in the list,
;;; and the second value is one of the keywords :proper, :dotted, and
;;; :circular.  For an atom, 0 and :dotted is returned.  
;;;
;;; This function is useful for processing code because lists
;;; representing code are not often very long, so the method used is
;;; fast and appropriate, and because we often need to check that such
;;; lists are proper, but the simple method would go into an infinite
;;; computation if the list is circular, whereas we would like to give
;;; an error message in that case.
(defun list-structure (object)
  ;; First we attempt to just traverse the list as usual,
  ;; assuming that it is fairly short.  If we reach the end,
  ;; then that's great, and we return the result.
  (loop for remaining = object then (cdr remaining)
	for count from 0 to 100
	while (consp remaining)
	finally (when (atom remaining)
		  (return-from list-structure
		    (values count
			    (if (null remaining)
				:proper
				:dotted)))))
  ;; Come here if the list has more than a few CONS cells.  We
  ;; traverse it again, this time entering each CONS cell in a hash
  ;; table.  Stop when we reach the end of the list, or when we see
  ;; the same CONS cell twice. 
  (let ((table (make-hash-table :test #'eq)))
    (loop for remaining = object then (cdr remaining)
	  while (consp remaining)
	  until (gethash remaining table)
	  do (setf (gethash remaining table) t)
	  finally (return (values (hash-table-count table)
				  (if (null remaining)
				      :proper
				      (if (atom remaining)
					  :dotted
					  :circular)))))))

(defun proper-list-p (object)
  (eq (nth-value 1 (list-structure object)) :proper))
