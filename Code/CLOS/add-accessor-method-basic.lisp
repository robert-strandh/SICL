(in-package #:sicl-clos)

;;;; This file contains very basic versions of the functions
;;;; ADD-READER-METHOD and ADD-WRITER-METHOD.  These functions are
;;;; called when a class metaobject is created that has one or more
;;;; slots with a :READER, :WRITER, or :ACCESSOR slot option.
;;;; Specifically, they are called from the :AFTER methods on
;;;; INITIALIZE-INSTANCE specialized for STANDARD-CLASS and
;;;; FUNCALLABLE-STANDARD-CLASS.
;;;;
;;;; The versions are very basic in that they directly apply the
;;;; semantics of reader and writer methods, i.e., they are turned
;;;; into calls to SLOT-VALUE and (SETF SLOT-VALUE).  
;;;;
;;;; These basic versions are quite slow, but they have the advantage
;;;; that they do not require the presence of the compiler.  This
;;;; feature can be useful for bootstrapping purposes, in that it
;;;; becomes possible to create a new class in a very basic system
;;;; that does not yet contain the compiler.  These versions could
;;;; also be used in a runtime in which it is not desirable to include
;;;; the compiler. 

;;; Given a slot name, return a reader method function that respects
;;; the default calling conventions of methods, i.e. the method
;;; function takes two arguments: the list of arguments to the generic
;;; function, and a list of next methods to be invoked by
;;; CALL-NEXT-METHOD.  The reader method function returned by this
;;; function closes over the slot name.  Since in the case of a reader
;;; method function, CALL-NEXT-METHOD is never invoked, the
;;; NEXT-METHODS argument is ignored.  
(defun make-reader-method-function (slot-name)
  (lambda (arguments next-methods)
    (declare (ignore next-methods))
    (slot-value (car arguments) slot-name)))

;;; Add a reader method to a generic function.  CLASS is a class
;;; metaobject that plays the role of specialize for the argument of
;;; the reader method.  FUNCTION-NAME is the name of the reader
;;; generic function to which the reader method should be added.
;;; SLOT-DEFINITION is a DIRECT-SLOT-DEFINITION metaobject that
;;; contains the slot name associated with the new reader method, and
;;; that is also used to determine the method class to be used for the
;;; reader method.  SLOT-DEFINITION is also stored in the new reader
;;; method for optimization purposes. 
(defun add-reader-method (class function-name slot-definition)
  (let* ((slot-name (slot-definition-name slot-definition))
	 (lambda-list '(object))
	 (generic-function (ensure-generic-function
			    function-name :lambda-list lambda-list))
	 (specializers (list class))
	 (method-function (make-reader-method-function slot-name))
	 (method-class (reader-method-class
			class slot-definition
			:lambda-list lambda-list
			:specializers specializers
			:function method-function
			:slot-definition slot-definition))
	 (method (make-instance method-class
		   :lambda-list lambda-list
		   :specializers specializers
		   :function method-function
		   :slot-definition slot-definition)))
    (add-method generic-function method)))

;;; Given a slot name, return a writer method function that respects
;;; the default calling conventions of methods, i.e. the method
;;; function takes two arguments: the list of arguments to the generic
;;; function, and a list of next methods to be invoked by
;;; CALL-NEXT-METHOD.  The writer method function returned by this
;;; function closes over the slot name.  Since in the case of a writer
;;; method function, CALL-NEXT-METHOD is never invoked, the
;;; NEXT-METHODS argument is ignored.
(defun make-writer-method-function (slot-name)
  (lambda (arguments next-methods)
    (declare (ignore next-methods))
    (setf (slot-value (cadr arguments) slot-name) (car arguments))))

;;; Add a writer method to a generic function.  CLASS is a class
;;; metaobject that plays the role of specialize for the argument of
;;; the writer method.  FUNCTION-NAME is the name of the writer
;;; generic function to which the writer method should be added.
;;; SLOT-DEFINITION is a DIRECT-SLOT-DEFINITION metaobject that
;;; contains the slot name associated with the new writer method, and
;;; that is also used to determine the method class to be used for the
;;; writer method.  SLOT-DEFINITION is also stored in the new writer
;;; method for optimization purposes.
(defun add-writer-method (class function-name slot-definition)
  (let* ((slot-name (slot-definition-name slot-definition))
	 (lambda-list '(new-value object))
	 (generic-function (ensure-generic-function
			    function-name :lambda-list lambda-list))
	 (specializers (list *t* class))
	 (method-function (make-writer-method-function slot-name))
	 (method-class (writer-method-class
			class slot-definition
			:lambda-list lambda-list
			:specializers specializers
			:function method-function
			:slot-definition slot-definition))
	 (method (make-instance method-class
				:lambda-list lambda-list
				:specializers specializers
				:function method-function
				:slot-definition slot-definition)))
    (add-method generic-function method)))


  
