(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ADD-DIRECT-SUBCLASS.
;;;
;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/add-direct-subclass.html
;;;
;;; The specification includes a single method on this generic
;;; function, specialized for CLASS and CLASS.

(defgeneric add-direct-subclass (superclass subclass))

(defmethod add-direct-subclass ((superclass class) (subclass class))
  (when (member subclass (class-direct-subclasses superclass))
    (error 'attempt-to-add-existing-subclass
           :subclass subclass
           :superclass superclass))
  (setf (class-direct-subclasses superclass)
        (cons subclass (class-direct-subclasses superclass))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function REMOVE-DIRECT-SUBCLASS.
;;;
;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/remove-direct-subclass.html
;;;
;;; The specification includes a single method on this generic
;;; function, specialized for CLASS and CLASS.

(defgeneric remove-direct-subclass (superclass subclass))

(defmethod remove-direct-subclass ((superclass class) (subclass class))
  (setf (class-direct-subclasses superclass)
        (remove subclass (class-direct-subclasses superclass)
                :test #'eq)))
