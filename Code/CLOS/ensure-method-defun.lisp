(cl:in-package #:sicl-clos)

;;; This function is not required by the standard.
;;;
;;; GENERIC-FUNCTION is either a generic-function metaobject, or the
;;; name of a generic function.  If it is a name, then FBOUNDP,
;;; FDEFINITION, etc., are called in order to determine whether a
;;; generic function with hat name exists.  If it exists, then it is
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
;;; of the form (SETF <object>) (designating an EQL specializer with
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
;;;
;;; SLOT-DEFINITION is a slot-definition metaobject.  If METHOD-CLASS
;;; is a subclass of the class STANDARD-METHOD, and this argument is
;;; not supplied, an error is signaled.  If this argument is supplied,
;;; it must be a subclass of the class named DIRECT-SLOT-DEFINITION.

(defun ensure-specializers (specializer-designators)
  (loop for specializer-designator in specializer-designators
        collect (etypecase specializer-designator
                  (specializer
                   specializer-designator)
                  (symbol
                   (find-class specializer-designator))
                  ((cons (eql eql) (cons t null))
                   (make-instance 'eql-specializer
                     :object (second specializer-designator))))))

(defun ensure-method
    (generic-function-or-name
     &key
       (generic-function-class
        'standard-generic-function generic-function-class-p)
       (method-class 'standard-method)
       (lambda-list nil lambda-list-p)
       (qualifiers '())
       (specializers nil specializers-p)
       (documentation nil)
       (function nil function-p)
       (slot-definition nil slot-definition-p))
  (let ((generic-function
          (cond ((typep generic-function-or-name 'generic-function)
                 generic-function-or-name)
                ((symbolp generic-function-or-name)
                 (cond ((not (fboundp generic-function-or-name))
                        nil)
                       ((special-operator-p generic-function-or-name)
                        (error 'name-refers-to-a-special-operator
                               :name generic-function-or-name))
                       ((not (null (macro-function generic-function-or-name)))
                        (error 'name-refers-to-a-macro
                               :name generic-function-or-name))
                       (t
                        (fdefinition generic-function-or-name))))
                ((typep generic-function-or-name
                        '(cons (eql setf) (cons symbol null)))
                 (if (fboundp generic-function-or-name)
                     (fdefinition generic-function-or-name)
                     nil))
                (t
                 (error 'invalid-function-name
                        :name generic-function-or-name)))))
    (when (null generic-function)
      (setf generic-function
            (if generic-function-class-p
                (ensure-generic-function
                 generic-function-or-name
                 ;; FIXME: This is wrong.  The default values for &key
                 ;; and &optional must be removed, and the &AUX
                 ;; variables must all be removed.
                 :lambda-list (extract-lambda-list lambda-list)
                 :generic-function-class generic-function-class
                 :method-class method-class)
                (ensure-generic-function
                 generic-function-or-name
                 ;; FIXME: This too is wrong.
                 :lambda-list (extract-lambda-list lambda-list)
                 :method-class method-class))))
    (let ((method
            (make-instance method-class
              :lambda-list (extract-lambda-list lambda-list)
              :qualifiers qualifiers
              :specializers (ensure-specializers specializers)
              :documentation documentation
              :function function)))
      (add-method generic-function method)
      method)))

;;  LocalWords:  specializer specializers SPECIALIZERS designators
;;  LocalWords:  designator metaobject
