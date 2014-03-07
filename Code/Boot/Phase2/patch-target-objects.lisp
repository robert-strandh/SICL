(cl:in-package #:sicl-boot-phase2)

(defun already-patched-p (heap-instance)
  (heap-instance-p
   (heap-instance-class heap-instance)))

(defun unpatched-built-in-instance-p (heap-instance)
  (and (not (already-patched-p heap-instance))
       (eq (cl:class-of (heap-instance-class heap-instance))
	   (cl:find-class 'built-in-class))))

(defun patch-standard-object (object)
  (unless (already-patched-p object)
    (let ((bridge-class (heap-instance-class object)))
      (let* ((name (sicl-boot-phase1::class-name bridge-class))
	     (target-class (find-target-class name)))
	(setf (heap-instance-class object)
	      target-class)
	(setf (standard-instance-access object 1)
	      (class-slots target-class))))))

(defun patch-built-in-instance (object)
  (unless (already-patched-p object)
    (let ((bridge-class (heap-instance-class object)))
      (let* ((name (sicl-boot-phase1::class-name bridge-class))
	     (target-class (find-target-class name)))
	(setf (heap-instance-class object)
	      target-class)))))

(defun patch-target-objects ()
  (let ((seen '()))
    (labels ((traverse (object)
	       (unless (member object seen)
		 (push object seen)
		 (typecase object
		   (heap-instance
		    (unless (already-patched-p object)
		      (if (unpatched-built-in-instance-p object)
			  (patch-built-in-instance object)
			  (patch-standard-object object)))
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
	    do (traverse obj)))))
