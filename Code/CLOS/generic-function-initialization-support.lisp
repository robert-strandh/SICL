(in-package #:sicl-clos)

(defun check-argument-precedence-order
    (argument-precedence-order required-parameters)
  (unless (proper-list-p argument-precedence-order)
    (error "argument-precedence-order must be a proper list"))
  (unless (and (= (length argument-precedence-order)
		  (length required-parameters))
	       (null (set-difference argument-precedence-order
				     required-parameters))
	       (null (set-difference required-parameters
				     argument-precedence-order)))
    (error "argument precedence order must be a permutation~@
            of the required parameters")))

(defun check-documentation (documentation)
  (unless (or (null documentation) (stringp documentation))
    (error "documentation must be NIL or a string")))

;;; FIXME: check the syntax of each declaration. 
(defun check-declarations (declarations)
  (unless (proper-list-p declarations)
    (error "declarations must be a proper list")))

(defun check-method-combination (method-combination)
  ;; FIXME: check that method-combination is a method-combination
  ;; metaobject.
  (declare (ignore method-combination))
  nil)

(defun set-method-class (generic-function method-class)
  ;; FIXME: check that the method-class is a subclss of METHOD.
  (setf (gf-method-class generic-function)
	method-class))

(defun initialize-instance-after-standard-generic-function-default
    (generic-function
     &key
       method-combination
       (method-class (find-class 'standard-method))
       (name nil)
     &allow-other-keys)
  ;; FIXME: handle different method combinations.
  (declare (ignore method-combination))
  (set-method-class generic-function method-class)
  (let ((fun (compute-discriminating-function generic-function)))
    ;; FIXME: The name test should not be necessary.
    (unless (null name)
      (set-funcallable-instance-function generic-function fun))))

(defun reinitialize-instance-after-standard-generic-function-default
    (generic-function
     &rest initargs
     &key
       method-combination
       (method-class nil method-class-p)
     &allow-other-keys)
  ;; FIXME: handle different method combinations.
  (declare (ignore method-combination))
  (when method-class-p
    (set-method-class generic-function method-class))
  (map-dependents
   generic-function
   (lambda (dependent)
     (apply #'update-dependent generic-function dependent initargs))))
