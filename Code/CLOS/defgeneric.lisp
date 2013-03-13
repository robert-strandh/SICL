(in-package #:sicl-clos-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ENSURE-GENERIC-FUNCTION, ENSURE-GENERIC-FUNCTION-USING-CLASS.

(defgeneric ensure-generic-function-using-class
    (generic-function
     function-name
     &key
       argument-precedence-order
       declarations
       documentation
       generic-function-class
       lambda-list
       method-class
       method-combination
       name
     &allow-other-keys)

(defmethod ensure-generic-function-using-class
    ((generic-function null)
     function-name
     &rest
       all-keyword-arguments
     &key
       (generic-function-class (find-class 'standard-generic-function))
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
    (if method-class-p
	(apply #'make-instance generic-function-class
	       :method-class method-class
	       remaining-keys)
	(apply #'make-instance generic-function-class
	       remaining-keys))))
  
(defmethod ensure-generic-function-using-class
    ((generic-function generic-function)
     function-name
     &rest
       all-keyword-arguments
     &key
       (generic-function-class (find-class 'standard-generic-function))
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
  (unless (eq generic-function-class (class-of generic-function))
    (error "classes don't agree"))
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
    (if method-class-p
	(apply #'reinitialize-instance generic-function
	       :method-class method-class
	       remaining-keys)
	(apply #'reinitialize-instance generic-function
	       remaining-keys)))
  generic-function)

(defun ensure-generic-function (name &rest keys)
  ;; FIXME: check if it names a non-generic function or macro already. 
  (let ((generic-function (gethash name *generic-functions*)))
    (apply #'ensure-generic-function-using-class generic-function
	   keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DEFGENERIC.

;;; FIXME: add options and methods
(defmacro defgeneric (name lambda-list)
  `(ensure-generic-function
    ',name
    :lambda-list ',lambda-list))

