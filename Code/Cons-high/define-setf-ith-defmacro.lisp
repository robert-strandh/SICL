(cl:in-package #:sicl-cons-high)

(defmacro define-setf-ith (name i)
  `(defun (setf ,name) (new-value list)
     (let ((cell (loop for remaining = list then (rest remaining)
		       repeat ,(1- i)
		       unless (consp remaining)
			 (error 'expected-list-with-at-least-n-elements
				:found found
				:at-least ,i)
		       finally (return remaining))))
       (rplaca cell new-value)
       new-value)))
