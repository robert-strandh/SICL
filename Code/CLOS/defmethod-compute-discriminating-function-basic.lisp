(cl:in-package #:sicl-clos)

;;;; This file contains a definition of the specified method on the
;;;; generic function COMPUTE-DISCRIMINATING-FUNCTION specialized for
;;;; STANDARD-GENERIC-FUNCTION.  This particular definition is very
;;;; basic, in that it does no caching whatsoever.  Instead, it always
;;;; calls COMPUTE-APPLICABLE-METHODS and COMPUTE-EFFECTIVE-METHOD and
;;;; calls the resulting effective method.
;;;;
;;;; Clearly, this method is very slow, but it is useful because it
;;;; does not invoke the compiler, which we use for bootstrapping
;;;; purposes.  This method COULD be used for applications where it is
;;;; not practical to include the compiler at runtime, but it would
;;;; very likely be unacceptably slow.

;;; We need to test for a few particular generic functions, namely
;;; COMPUTE-APPLICABLE-METHODS, COMPUTE-EFFECTIVE-METHOD, and
;;; COMPUTE-DISCRIMINATING-FUNCTION so that they can be treated
;;; specially.  We do this by having global variables containing
;;; references to those functions.  During early stages of
;;; bootstrapping, these functions exist only has host generic
;;; functions, and the functions passed as argument to
;;; COMPUTE-DISCRIMINATING-FUNCTION will all be readers and writers,
;;; so we know they are not one of the ones that need special
;;; treatment.  For that reason, we give these variables initial
;;; values (NIL) so that they can be tested for, but the test will
;;; always fail during early stages of bootstrapping.

(defparameter *generic-function-compute-applicable-methods* nil)
(defparameter *generic-function-compute-effective-method* nil)
(defparameter *generic-function-compute-discriminating-function* nil)

;;; This function takes a generic function an returns a default
;;; discriminating function for it.
(defun make-default-discriminating-function (generic-function)
  (lambda (&rest arguments)
    (let* ((applicable-methods
	     (compute-applicable-methods generic-function arguments))
	   (method-combination
	     (generic-function-method-combination generic-function))
	   (effective-method
	     (compute-effective-method generic-function
				       method-combination
				       applicable-methods)))
      (funcall effective-method arguments))))

;;; The generic function COMPUTE-APPLICABLE-METHODS needs a special
;;; discriminating function, because we don't want to invoke
;;; COMPUTE-APPLICABLE-METHODS in order to compute the applicable
;;; methods of COMPUTE-APPLICABLE-METHODS.
(defun make-df-for-compute-applicable-methods (generic-function)
  (let ((default-discriminating-function
	  (make-default-discriminating-function generic-function)))
    (lambda (&rest arguments)
      (if (eq (class-of (first arguments)) *standard-generic-function*)
	  (apply #'compute-applicable-methods-default arguments)
	  (apply default-discriminating-function arguments)))))
  
;;; The generic function COMPUTE-EFFECTIVE-METHOD needs a special
;;; discriminating function, because we don't want to invoke
;;; COMPUTE-EFFECTIVE-METHOD in order to compute the effective method
;;; of COMPUTE-EFFECTIVE-METHOD.
(defun make-df-for-compute-effective-method (generic-function)
  (let ((default-discriminating-function
	  (make-default-discriminating-function generic-function)))
    (lambda (&rest arguments)
      (if (and (eq (class-of (first arguments)) *standard-generic-function*)
	       (eq (class-of (second arguments)) *standard-method-combination*))
	  (compute-effective-method-default (third arguments))
	  (apply default-discriminating-function arguments)))))
  
;;; The generic function COMPUTE-DISCRIMINATING-FUNCTION needs a
;;; special discriminating function, because we don't want to invoke
;;; COMPUTE-DISCRIMINATING-FUNCTION in order to compute the
;;; discriminating function of COMPUTE-DISCRIMINATING-FUNCTION.
(defun make-df-for-compute-discriminating-function (generic-function)
  (let ((default-discriminating-function
	  (make-default-discriminating-function generic-function)))
    (lambda (&rest arguments)
      (if (eq (class-of (first arguments)) *standard-generic-function*)
	  (apply #'compute-discriminating-function-default arguments)
	  (apply default-discriminating-function arguments)))))
  
;;;
(defun compute-discriminating-function-default (generic-function)
  (cond ((eq generic-function
	     *generic-function-compute-applicable-methods*)
	 (make-df-for-compute-applicable-methods generic-function))
	((eq generic-function
	     *generic-function-compute-effective-method*)
	 (make-df-for-compute-effective-method generic-function))
	((eq generic-function
	     *generic-function-compute-discriminating-function*)
	 (make-df-for-compute-discriminating-function generic-function))
	(t
	 (make-default-discriminating-function generic-function))))

(defmethod compute-discriminating-function
    ((generic-function standard-generic-function))
  (compute-discriminating-function-default generic-function))

