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
			   (apply *call-next-method* args))
			 (next-method-p ()
			   (funcall *next-method-p*)))
		    ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPUTE-APPLICABLE-METHODS.

;;; Class C1 is a sub-specizlizer of class C2 with respect to some
;;; argument class C if and only if C1 occurs before C2 in the class
;;; precedence list of C.
;;;
;;; Recall that this function is used to determine whether one
;;; applicable method is more specific than another applicable method.
;;; Thus, we have already determined that C is more specific than both
;;; C1 and C2, and therefore both C1 and C2 are in the class
;;; precedence list of C,
;;;
;;; Should we ever be called with classes C1 and C2 that are not in
;;; the class precedence list of C, then the method we use (numeric
;;; comparison of the result of calling POSITION), will signal an
;;; error, which is reassuring.
(defun sub-specializer-p (class1 class2 class-of-argument)
  (let ((precedence-list (class-precedence-list class-of-argument)))
    (< (position class1 precedence-list) (position class2 precedence-list))))

;;; Determine whether a method is more specific than another method
;;; with respect to a list of classes of required arguments.  
;;;
;;; Recall that whether a method is more or less specific than another
;;; method is also a function of the classes of the arguments, because
;;; the order of two classes in the class precedence list of two
;;; different argument classes can be different.  
;;;
;;; This function is called only with applicable methods with respect
;;; to the classes of the arguments supplied.  
;;;
;;; It is possible for two methods of a generic function to be equally
;;; specific (which then means that they have the same specializer in
;;; every required position), but then they must have different
;;; qualifiers.  This function is called with all applicable
;;; functions, independent of the qualifiers, so this situation might
;;; happen here.
;;;
;;; FIXME: take into account the argument precedence order.
(defun method-more-specific-p (method1 method2 classes-of-arguments)
  (loop for s1 in (method-specializers method1)
	for s2 in (method-specializers method2)
	for class-of-argument in classes-of-arguments
	unless (eq s1 s2)
	  return (sub-specializer-p s1 s2 class-of-argument)))

;;; Determine whether a class C1 is a subclass of another class C2.
;;; This can be done by checking whether C2 is in the class precedence
;;; list of C1.
(defun subclassp (class1 class2)
  (member class2 (class-precedence-list class1)))

;;; Given a list of classes of the required arguments of a generic
;;; function, compute the applicable methods, independently of their
;;; qualifiers, but sorted in order from most to least specific. 
;;;
;;; The applicable methods are found by filtering out methods for
;;; which every specializer is a (non-strict) subclass of the
;;; corresponding argument class.  Then they are sorted according to
;;; the order determined by METHOD-MORE-SPECIFIC-P as defined above. 
(defun compute-applicable-methods-using-classes
    (generic-function classes-of-arguments)
  (sort (copy-list
	 (remove-if-not (lambda (method)
			  (every #'subclassp
				 classes-of-arguments
				 (method-specializers method)))
			(generic-function-methods generic-function)))
	(lambda (method1 method2)
	  (method-more-specific-p method1 method2 classes-of-arguments))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPUTE-EFFECTIVE-METHOD-FUNCTION.

(defun primary-method-p (method)
  (null (method-qualifiers method)))

(defun after-method-p (method)
  (equal (method-qualifiers method) '(:after)))

(defun before-method-p (method)
  (equal (method-qualifiers method) '(:before)))

(defun wrap-methods (methods)
  (if (null (cdr methods))
      `(let ((*call-next-method*
	       (lambda (&rest new-args)
		 (declare (ignore new-args))
		 (error "no next method")))
	     (*next-method-p* (lambda () nil))
	     (args (or new-args args)))
	 (format *trace-output* "args: ~s~%" args)
	 (apply ,(method-function (car methods)) args))
      `(let ((*call-next-method*
	       (lambda (&rest new-args)
		 ,(wrap-methods (cdr methods))))
	     (*next-method-p* (lambda () t))
	     (args (or new-args args)))
	 (format *trace-output* "args: ~s~%" args)
	 (apply ,(method-function (car methods)) args))))

(defun compute-effective-method-function (methods)
  (let ((primary-methods (remove-if-not #'primary-method-p methods))
	(before-methods (remove-if-not #'before-method-p methods))
	(after-methods (remove-if-not  #'after-method-p methods)))
    `(lambda (args)
       ,@(loop for method in before-methods
	       collect `(apply ,(method-function method) args))
       (multiple-value-prog1
	   (let ((new-args '()))
	     ,(wrap-methods primary-methods))
	 ,@(loop for method in (reverse after-methods)
		 collect `(apply ,(method-function method) args))))))
	     
