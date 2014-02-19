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

(defun check-method-class (method-class)
  ;; FIXME: check that the method-class is a subclss of METHOD.
  (declare (ignore method-class))
  nil)

(defun initialize-instance-after-standard-generic-function-default
    (generic-function
     &key
     &allow-other-keys)
  (let ((fun (compute-discriminating-function generic-function)))
    (set-funcallable-instance-function generic-function fun)))

(defun reinitialize-instance-after-standard-generic-function-default
    (generic-function
     &rest initargs
     &key
     &allow-other-keys)
  (map-dependents
   generic-function
   (lambda (dependent)
     (apply #'update-dependent generic-function dependent initargs))))
