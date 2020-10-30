(cl:in-package #:sicl-clos)

;;;; This file contains very basic versions of the functions
;;;; ADD-READER-METHOD and ADD-WRITER-METHOD.  These functions are not
;;;; part of the specification, but they convenient abstractions that
;;;; are called when a class metaobject is created that has one or
;;;; more slots with a :READER, :WRITER, or :ACCESSOR slot option.
;;;; Specifically, they are called from the :AFTER methods on
;;;; INITIALIZE-INSTANCE specialized for STANDARD-CLASS and
;;;; FUNCALLABLE-STANDARD-CLASS.

;;; Add a reader method to a generic function.  CLASS is a class
;;; metaobject that plays the role of specializer for the argument of
;;; the reader method.  FUNCTION-NAME is the name of the reader
;;; generic function to which the reader method should be added.
;;; SLOT-DEFINITION is a DIRECT-SLOT-DEFINITION metaobject that
;;; contains the slot name associated with the new reader method, and
;;; that is also used to determine the method class to be used for the
;;; reader method.  SLOT-DEFINITION is also stored in the new reader
;;; method for optimization purposes.
(defun add-reader-method (class function-name slot-definition)
  (let* ((lambda-list '(object))
         (generic-function
           (ensure-generic-function
            function-name :lambda-list lambda-list))
         (specializers (list class))
         (slot-name (slot-definition-name slot-definition))
         (method-function
           (compile nil `(lambda (arguments next-methods)
                           (declare (ignore next-methods))
                           (slot-value (car arguments) ',slot-name))))
         (method-class (reader-method-class
                        class slot-definition
                        :lambda-list lambda-list
                        :qualifiers '()
                        :specializers specializers
                        :function method-function
                        :slot-definition slot-definition))
         (method (make-instance method-class
                   :lambda-list lambda-list
                   :qualifiers '()
                   :specializers specializers
                   :function method-function
                   :slot-definition slot-definition)))
    (add-method generic-function method)))

;;; Add a writer method to a generic function.  CLASS is a class
;;; metaobject that plays the role of specializer for the argument of
;;; the writer method.  FUNCTION-NAME is the name of the writer
;;; generic function to which the writer method should be added.
;;; SLOT-DEFINITION is a DIRECT-SLOT-DEFINITION metaobject that
;;; contains the slot name associated with the new writer method, and
;;; that is also used to determine the method class to be used for the
;;; writer method.  SLOT-DEFINITION is also stored in the new writer
;;; method for optimization purposes.
(defun add-writer-method (class function-name slot-definition)
  (let* ((lambda-list '(new-value object))
         (generic-function
           (ensure-generic-function
            function-name :lambda-list lambda-list))
         (specializers (list (find-class 't) class))
         (slot-name (slot-definition-name slot-definition))
         (method-function
           (compile nil `(lambda (arguments next-methods)
                           (declare (ignore next-methods))
                           (setf (slot-value (cadr arguments) ',slot-name)
                                 (car arguments)))))
         (method-class (writer-method-class
                        class slot-definition
                        :lambda-list lambda-list
                        :qualifiers '()
                        :specializers specializers
                        :function method-function
                        :slot-definition slot-definition))
         (method (make-instance method-class
                                :lambda-list lambda-list
                                :qualifiers '()
                                :specializers specializers
                                :function method-function
                                :slot-definition slot-definition)))
    (add-method generic-function method)))
