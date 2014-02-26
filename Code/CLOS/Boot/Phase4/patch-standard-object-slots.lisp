(cl:in-package #:sicl-clos)

(let ((seen '()))
  (labels ((traverse (object)
	     (unless (member object seen)
	       (push object seen)
	       (typecase object
		 (heap-instance
		  (let ((class (heap-instance-class object)))
		    (setf (standard-instance-access object 1)
			  (class-slots class))
		    (traverse class)
		    (traverse (heap-instance-slots object))))
		 (cons
		  (traverse (car object))
		  (traverse (cdr object)))
		 (vector
		  (loop for element across object
			do (traverse element)))
		 (t
		  nil)))))
    (loop for obj in (append *target-generic-functions*
			     *target-classes*)
	  do (traverse obj))))
