(cl:in-package #:sicl-clos)

;;;; This file contains a version of the implementations of the
;;;; function MAKE-READER-METHOD-FUNCTION for creating the function
;;;; used in a reader method and MAKE-WRITER-METHOD-FUNCTION for
;;;; creating the function used in a writer method.  These functions
;;;; are called by ADD-READER-METHOD and ADD-WRITER-METHOD.
;;;;
;;;; The version is very basic in that they directly apply the
;;;; semantics of reader and writer methods, i.e., they are turned
;;;; into calls to SLOT-VALUE and (SETF SLOT-VALUE).
;;;;
;;;; The functions defined here require the presence of the compiler.
;;;; They can be useful because they can be created by the cross
;;;; compiler.  This feature can be useful for bootstrapping purposes.

;;; Given a slot name, return a reader method function that respects
;;; the default calling conventions of methods, i.e. the method
;;; function takes two arguments: the list of arguments to the generic
;;; function, and a list of next methods to be invoked by
;;; CALL-NEXT-METHOD.  The reader method function returned by this
;;; function generates a compiled function in which the slot name is a
;;; constant.  Since in the case of a reader method function,
;;; CALL-NEXT-METHOD is never invoked, the NEXT-METHODS argument is
;;; ignored.
(defun make-reader-method-function (slot-name)
  (compile nil
           `(lambda (arguments next-methods)
              (declare (ignore next-methods))
              (slot-value (car arguments) ',slot-name))))

;;; Given a slot name, return a writer method function that respects
;;; the default calling conventions of methods, i.e. the method
;;; function takes two arguments: the list of arguments to the generic
;;; function, and a list of next methods to be invoked by
;;; CALL-NEXT-METHOD.  The writer method function returned by this
;;; function generates a compiled function in which the slot name is a
;;; constant.  Since in the case of a writer method function,
;;; CALL-NEXT-METHOD is never invoked, the NEXT-METHODS argument is
;;; ignored.
(defun make-writer-method-function (slot-name)
  (compile nil
           `(lambda (arguments next-methods)
              (declare (ignore next-methods))
              (setf (slot-value (cadr arguments) ',slot-name)
                    (car arguments)))))
