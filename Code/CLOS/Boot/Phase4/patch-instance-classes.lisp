(cl:in-package #:sicl-clos)

(let ((seen '()))
  (labels ((traverse (object)
	     (unless (member object seen)
	       (push object seen)
	       (typecase object
		 (heap-instance
		  (let ((class (heap-instance-class object)))
		    (unless (heap-instance-p class)
		      (let ((target-class-entry
			      (find (class-name class) *target-classes*
				    :key #'car)))
			(assert (not (null target-class-entry)))
			(setf (heap-instance-class object)
			      (cdr target-class-entry))))
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
