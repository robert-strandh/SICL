(cl:in-package #:sicl-cons-high)

;;;FIXME: use the new improved condition instead.
(defmacro define-setf-c*r-function (function-name letters)
  (flet ((primitive (letter)
	   (if (eql letter #\A) 'car 'cdr)))
    (flet ((one-iteration (letter)
	     `(progn (if (consp remaining)
			 (setf remaining
			       (,(primitive letter) remaining))
			 (error 'must-be-cons
				:datum remaining
				:name '(setf ,function-name))))))
      `(defun (setf ,function-name) (new-object list)
	 (let ((remaining list))
	   ,@(append (loop for i downfrom (1- (length letters)) to 0
			   for letter = (aref letters i)
			   collect (one-iteration letter))
		     `((if (consp remaining)
			   (setf (,(primitive (aref letters 0)) remaining)
				 new-object)
			   (error 'must-be-cons
				  :datum remaining
				  :name '(setf ,function-name))))))))))
