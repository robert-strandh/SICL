(cl:in-package #:sicl-clos)
;;;
;;; UNSPECIALIZED-LAMBDA-LIST is the unspecialized lambda list
;;; corresponding to the lambda list of the method to be created.  An
;;; error is signaled if this argument is not supplied, or if it is a
;;; malformed unspecialized lambda list.
;;;
;;; QUALIFIERS is a list of method qualifiers.  An error is signaled
;;; if this argument is not a proper list, or if any element of the
;;; list is not a non-null atom.  If this argument is not supplied, it
;;; defaults to the empty list.
;;;
;;; SPECIALIZER-DESIGNATORS is a list of specializer designators.  A
;;; specializer designator is either a specializer metaobject
;;; (designating itself), a symbol (designating a class with that
;;; name), or a list of the form (EQL <object>) (designating an EQL
;;; specializer with <object> as the specializer object. An error is
;;; signaled if this argument is not a proper list, or if any element
;;; of the list is not a specializer designator.  If this argument is
;;; not supplied, an error is signaled.
;;;
;;; DOCUMENTATION is a string or NIL.  An error is signaled if this
;;; argument is neither a string not NIL.  If this argument is not
;;; supplied, it defaults to NIL.
;;;
;;; FUNCTION is a method function.  If this argument is not supplied,
;;; an error is signaled.

(defgeneric ensure-method-using-generic-function
    (generic-function
     &key
       unspecialized-lambda-list
       qualifiers
       specializers
       documentation
       function
     &allow-other-keys))

;;; FIXME: check for errors according to comment above.

(defmethod ensure-method-using-generic-function
    ((generic-function standard-generic-function)
     &key
       unspecialized-lambda-list
       qualifiers
       specializers
       documentation
       function
     &allow-other-keys)
  (let* ((method-class
           (clostrophilia:generic-function-method-class generic-function))
         (method
           (make-instance method-class
             :lambda-list unspecialized-lambda-list
             :qualifiers qualifiers
             :specializers specializers
             :documentation documentation
             :function function)))
    (clostrophilia:add-method generic-function method)))
    
                               


