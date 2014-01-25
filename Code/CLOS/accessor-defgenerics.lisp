(cl:in-package #:sicl-clos)

;;; Specified.

;;; The name of a class can be any object, but it is usually a symbol.
;;; If it is NIL, then it means that the class does not have a name.
;;; For a subclass of STANDARD-CLASS, FUNCALLABLE-STANDARD-CLASS, or
;;; FORWARD-REFERENCED-CLASS, this function returns the defaulted value
;;; of the argument :NAME, given when the class was initialized or
;;; reinitialized.  For a subclass of BUILT-IN-CLASS, it returns the
;;; name of the built-in class. 
(defgeneric class-name (class))

;;; According to the AMOP, this function should call
;;; REINITIALIZE-INSTANCE with three arguments: 
;;; CLASS, :NAME, and NEW-NAME.
(defgeneric (setf class-name) (new-name class))

;;; Return the defaulted value of the argument :DIRECT-SUPERCLASSES
;;; given when the class was initialized or reinitialized.  The value
;;; is a list of class metaobjects, i.e. subclasses of the class
;;; CLASS.
(defgeneric class-direct-superclasses (class))

;;; Return the defaulted value of the argument :DIRECT-SLOTS given
;;; when the class was initialized or reinitialized.  The value is a
;;; list of direct slot definition metaobjects, i.e., subclasses of
;;; the class DIRECT-SLOT-DEFINITION. 
(defgeneric class-direct-slots (class))

;;; Return a SET (represented as alist, but with the elements in no
;;; particular order) of subclasses of the class.
(defgeneric class-direct-subclasses (class))

;;; The functions ADD-DIRECT-SUBCLASS and REMOVE-DIRECT-SUBCLASS are
;;; used to update the direct subclasses of a class, so this function
;;; is not part of the public interface and should be used only from
;;; these two functions. 
(defgeneric (setf class-direct-subclasses) (new-value class))

;;; FIXME: write more comments
(defgeneric class-default-initargs (class))

;;; FIXME: write more comments
(defgeneric class-slots (class))

;;; FIXME: write more comments
(defgeneric class-precedence-list (class))

;;; FIXME: write more comments
(defgeneric class-finalized-p (class))

;;; FIXME: write more comments
(defgeneric class-prototype (class))

(defgeneric generic-function-name (generic-function))
(defgeneric generic-function-lambda-list (generic-function))
(defgeneric generic-function-argument-precedence-order (generic-function))
(defgeneric generic-function-declarations (generic-function))
(defgeneric generic-function-method-class (generic-function))
(defgeneric generic-function-method-combination (generic-function))
(defgeneric generic-function-methods (generic-function))

(defgeneric method-function (method))
(defgeneric method-generic-function (method))
(defgeneric (setf method-generic-function) (method))
(defgeneric method-lambda-list (method))
(defgeneric (setf method-lambda-list) (method))
(defgeneric method-specializers (method))
(defgeneric (setf method-specializers) (method))
(defgeneric method-qualifiers (method))
(defgeneric (setf method-qualifiers) (method))
(defgeneric method-documentation (method))
(defgeneric (setf method-documentation) (new-documentation method))
(defgeneric accessor-method-slot-definition (accessor-method))

(defgeneric slot-definition-allocation (slot-definition))
(defgeneric slot-definition-initargs (slot-definition))
(defgeneric slot-definition-initform (slot-definition))
(defgeneric slot-definition-initfunction (slot-definition))
(defgeneric slot-definition-name (slot-definition))
(defgeneric slot-definition-type (slot-definition))

;;; SICL specific.

(defgeneric standard-instance-slots (standard-instance))
(defgeneric standard-instance-timestamp (standard-instance))
(defgeneric (setf gf-lambda-list) (new-lambda-list generic-function))
(defgeneric gf-documentation (generic-function))
(defgeneric (setf gf-documentation) (generic-function))
(defgeneric discriminating-function (generic-function))
(defgeneric (setf discriminating-function)
    (new-discriminating-function generic-function))
(defgeneric dependents (generic-function))
(defgeneric (setf dependents) (new-dependents generic-function))
(defgeneric (setf gf-argument-precedence-order)
    (new-argument-precedence-order generic-function))
(defgeneric (setf gf-declarations) (new-declarations generic-function))
(defgeneric (setf gf-method-class) (new-method-class generic-function))
(defgeneric (setf gf-method-combination) (new-method-combination generic-function))
(defgeneric (setf gf-methods) (new-methods generic-function))
(defgeneric call-history (generic-function))
(defgeneric (setf call-history) (new-call-history generic-function))
(defgeneric specializer-profile (generic-function))
(defgeneric (setf specializer-profile) (new-specializer-profile generic-function))
(defgeneric set-cache (generic-function))
(defgeneric (setf set-cache) (new-set-cache generic-function))
