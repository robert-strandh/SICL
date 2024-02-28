(cl:in-package #:sicl-clos)

;;; This function is not required by the standard.
;;;
;;; GENERIC-FUNCTION is either a generic-function metaobject, or the
;;; name of a generic function.  If it is a name, then FBOUNDP,
;;; FDEFINITION, etc., are called in order to determine whether a
;;; generic function with that name exists.  If it exists, then it is
;;; used as is.  If not, ENSURE-GENERIC-FUNCTION is called to turn the
;;; name into a generic function metaobject.
;;;
;;; GENERIC-FUNCTION-CLASS is either a class metaobject, or the name
;;; of a class.  If GENERIC-FUNCTION is a function name and this
;;; argument is supplied, it is passed as the :GENERIC-FUNCTION-CLASS
;;; keyword argument to ENSURE-GENERIC-FUNCTION.
;;;
;;; METHOD-CLASS is either a class metaobject, or the name of a class.
;;; Either way, this argument is passed as the first argument to
;;; MAKE-INSTANCE.  If this argument is not supplied, it defaults to
;;; the symbol STANDARD-METHOD.
;;;
;;; LAMBDA-LIST is the unspecialized lambda list corresponding to the
;;; lambda list of the method to be created.  An error is signaled if
;;; this argument is not supplied, or if it is a malformed
;;; unspecialized lambda list.
;;;
;;; QUALIFIERS is a list of method qualifiers.  An error is signaled
;;; if this argument is not a proper list, or if any element of the
;;; list is not a non-null atom.  If this argument is not supplied, it
;;; defaults to the empty list.
;;;
;;; SPECIALIZERS is a list of specializer designators.  A specializer
;;; designator is either a specializer metaobject (designating
;;; itself), a symbol (designating a class with that name), or a list
;;; of the form (EQL <object>) (designating an EQL specializer with
;;; <object> as the specializer object. An error is signaled if this
;;; argument is not a proper list, or if any element of the list is
;;; not a specializer designator.  If this argument is not supplied,
;;; an error is signaled.
;;;
;;; DOCUMENTATION is a string or NIL.  An error is signaled if this
;;; argument is neither a string not NIL.  If this argument is not
;;; supplied, it defaults to NIL.
;;;
;;; FUNCTION is a method function.  If this argument is not supplied,
;;; an error is signaled.

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
