(defun recursive-find (x list)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (if (null list)
      nil
      (progn (recursive-find x (cdr list))
	     (when (eq (car list) x)
	       (return-from recursive-find x)))))
