(cl:in-package #:sicl-clos)

;;; This function is not required by the standard.
;;;
;;; GENERIC-FUNCTION is either a generic-function metaobject, or the
;;; name of a generic function.  If it is a name, then FBOUNDP,
;;; FDEFINITION, etc., are called in order to determine whether a
;;; generic function with that name exists.  If it exists, then it is
;;; used as is.  If not, ENSURE-GENERIC-FUNCTION is called to turn the
;;; name into a generic function metaobject.

(defun ensure-method
    (generic-function-or-name
     &key
       (method-class 'standard-method)
       (lambda-list nil lambda-list-p)
       (qualifiers '())
       (specializers nil specializers-p)
       (documentation nil)
       (function nil function-p))
  (let ((generic-function
          (if (or (symbolp generic-function-or-name)
                  (consp  generic-function-or-name))
              (ensure-generic-function generic-function-or-name)
              generic-function-or-name)))
    (let ((method
            (^make-instance method-class
              :lambda-list (extract-lambda-list lambda-list)
              :qualifiers qualifiers
              :specializers specializers
              :documentation documentation
              :function function)))
      (add-method generic-function method)
      method)))

;;  LocalWords:  specializer specializers SPECIALIZERS designators
;;  LocalWords:  designator metaobject
