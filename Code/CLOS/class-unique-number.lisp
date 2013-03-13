(in-package #:sicl-clos-test)

(defparameter *next-available-class-number* 0)

(defmethod class-unique-number :around ((class class))
  (or (call-next-method)
      (progn (loop for superclass in (class-direct-superclasses class)
		   do (class-unique-number superclass))
	     (prog1 (setf (class-unique-number class)
			  *next-available-class-number*)
	       (incf *next-available-class-number*)))))

		      