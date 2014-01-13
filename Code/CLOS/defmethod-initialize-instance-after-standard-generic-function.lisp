(cl:in-package #:sicl-clos)

;;; FIXME: check whether these methods should specialize on
;;; GENERIC-FUNCTION rather than on STANDARD-GENERIC-FUNCTION.

(defmethod initialize-instance :after
    ((generic-function standard-generic-function)
     &key
       (lambda-list nil lambda-list-p)
       (argument-precedence-order nil argument-precedence-order-p)
       documentation
       declarations
       method-combination
       (method-class (find-method-class 'standard-method))
       (name nil)
     &allow-other-keys)
  ;; FIXME: handle different method combinations.
  (declare (ignore method-combination))
  (set-documentation generic-function documentation)
  (set-declarations generic-function declarations)
  (set-method-class generic-function method-class)
  (set-lambda-list-and-argument-precedence-order
   generic-function
   lambda-list
   lambda-list-p
   argument-precedence-order
   argument-precedence-order-p)
  (let ((fun (lambda (&rest args)
	       (declare (ignore args))
	       (error "no applicable methods"))))
    (setf (discriminating-function generic-function) fun)
    (setf (gf-name generic-function) name)
    (unless (null name)
      (set-funcallable-instance-function fun generic-function))))

(defmethod reinitialize-instance :after
    ((generic-function standard-generic-function)
     &key
       (lambda-list nil lambda-list-p)
       (argument-precedence-order nil argument-precedence-order-p)
       (documentation nil documentation-p)
       (declarations nil declarations-p)
       method-combination
       (method-class nil method-class-p)
       (name nil name-p)  ; FIXME: check if this belongs here.
     &allow-other-keys)
  ;; FIXME: handle different method combinations.
  (declare (ignore method-combination))
  (when documentation-p
    (set-documentation generic-function documentation))
  (when declarations-p
    (set-declarations generic-function declarations))
  (when method-class-p
    (set-method-class generic-function method-class))
  (when name-p
    (setf (gf-name generic-function) name))
  (set-lambda-list-and-argument-precedence-order
   generic-function
   lambda-list
   lambda-list-p
   argument-precedence-order
   argument-precedence-order-p)
  (map-dependents class
		  (lambda (dependent)
		    (apply #'update-dependent class dependent args))))


  
