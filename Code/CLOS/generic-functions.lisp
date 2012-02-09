(in-package #:sicl-clos)

(defparameter *generic-functions* (make-hash-table :test #'eq))

(defparameter *call-next-method*
  (lambda (&rest args)
    (declare (ignore args))
    (error "not next method")))

(defparameter *next-method-p*
  (lambda () nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLASS-OF.

(defun class-of (object)
  (if (standard-instance-p object)
      (standard-instance-class object)
      (built-in-class-of object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ENSURE-GENERIC-FUNCTION and DEFGENERIC.

(defun ensure-generic-function (name &rest keys)
  (or (gethash name *generic-functions*)
      (let ((generic-function (apply #'make-instance
				     'standard-generic-function
				     :name name
				     keys)))
	(setf (gethash name *generic-functions*)
	      generic-function)
	generic-function)))

;;; FIXME: add options and methods
(defmacro defgeneric (name lambda-list)
  `(ensure-generic-function
    ',name
    :lambda-list ',lambda-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ENSURE-METHOD and DEFMETHOD

(defun parse-defmethod (args)
  (let ((name (pop args))
	(qualifiers (loop while (and (consp args) (not (listp (car args))))
			  collect (pop args)))
	(lambda-list (pop args)))
    ;; FIXME: handle declarations and documentation
    ;; FIXME: check the lambda list.  proper list, etc. 
    (let ((pos (position-if (lambda (x)
			      (member x '(&optional &rest &body &key
					  &allow-other-keys &aux)))
			    lambda-list)))
      (when (null pos)
	(setf pos (length lambda-list)))
      (values name
	      qualifiers
	      (append (mapcar (lambda (parameter)
				(if (consp parameter)
				    (car parameter)
				    parameter))
			      (subseq lambda-list 0 pos))
		      (subseq lambda-list pos))
	      (mapcar (lambda (parameter)
			(if (consp parameter)
			    (cadr parameter)
			    t))
		      (subseq lambda-list 0 pos))
	      args))))

(defun add-method (generic-function method)
  (setf (method-generic-function method) generic-function)
  (push method (generic-function-methods generic-function))
  ;; FIXME: add method to specializer classes
  method)

(defun ensure-method (generic-function &rest keys)
  (let ((method (apply #'make-instance
		       'standard-method
		       keys)))
    (add-method generic-function method)
    method))

(defun canonicalize-specializers (specializers)
  ;; FIXME: handle eql specializers.
  (mapcar #'find-class specializers))

(defmacro defmethod (&rest arguments)
  (multiple-value-bind (name qualifiers lambda-list specializers body)
      (parse-defmethod arguments)
    `(ensure-method
      (ensure-generic-function ',name)
      :lambda-list ',lambda-list
      :qualifiers ',qualifiers
      :specializers ',(canonicalize-specializers specializers)
      :body ',body
      :function (lambda ,lambda-list
		  (flet ((call-next-method (&rest args)
			   (funcall *call-next-method* args))
			 (next-method-p ()
			   (funcall *next-method-p*)))
		    ,@body)))))
