(cl:in-package #:sicl-clos)

(defun patch-standard-object (object)
  (let ((bridge-class (heap-instance-class object)))
    (if (heap-instance-p bridge-class)
	(error "this should not happen")
	(let* ((name (class-name bridge-class))
	       (target-class (find-target-class name))
	       (gf (find-bridge-generic-function 'class-slots)))
	  (setf (heap-instance-class object)
		target-class)
	  (setf (standard-instance-access object 1)
		(funcall gf target-class))))))

(defun patch-built-in-instance (object)
  (let ((bridge-class (heap-instance-class object)))
    (if (heap-instance-p bridge-class)
	(error "this should not happen")
	(let* ((name (class-name bridge-class))
	       (target-class (find-target-class name)))
	  (setf (heap-instance-class object)
		target-class)))))

(let ((seen '()))
  (labels ((traverse (object)
	     (unless (member object seen)
	       (push object seen)
	       (typecase object
		 (heap-instance
		  (patch-standard-object object)
		  (traverse (heap-instance-class object))
		  (traverse (heap-instance-slots object)))
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
