(cl:in-package #:sicl-clos)

(defmethod ensure-generic-function-using-class
    ((generic-function null)
     function-name
     &rest
       all-keyword-arguments
     &key
       (generic-function-class
	(find-generic-function-class 'standard-generic-function))
       (method-class nil method-class-p)
     &allow-other-keys)
  (cond ((symbolp generic-function-class)
	 (let ((class (find-class generic-function-class nil)))
	   (when (null class)
	     (error "no such generic-function-class ~s"
		    generic-function-class))
	   (setf generic-function-class class)))
	((classp generic-function-class)
	 nil)
	(t
	 (error "generic function class must be a class or a name")))
  (when method-class-p
    (cond ((symbolp method-class)
	   (let ((class (find-class method-class nil)))
	     (when (null class)
	       (error "no such method class ~s" method-class))
	     (setf method-class class)))
	  ((classp method-class)
	   nil)
	  (t
	   (error "method class must be a class or a name"))))
  (let ((remaining-keys (copy-list all-keyword-arguments)))
    (loop while (remf remaining-keys :generic-function-class))
    (let ((result (if method-class-p
		      (apply #'make-instance generic-function-class
			     ;; The AMOP does
			     :name function-name
			     :method-class method-class
			     remaining-keys)
		      (apply #'make-instance generic-function-class
			     :name function-name
			     remaining-keys))))
      ;; According to the AMOP, the name of the generic function is
      ;; set here, rather than by INITIALIZE-INSTANCE, but in all the
      ;; examples (chapter 1 and Closette), the :name keyword argument
      ;; is also passed to MAKE-INSTANCE.
      (setf (gf-name result) function-name)
      result)))
