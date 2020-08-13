(cl:in-package #:sicl-structure)

;;; These classes represent a defstruct form after parsing & validation.

(defclass defstruct-description ()
  ((%name :initarg :name :reader defstruct-name)
   (%documentation :initarg :documentation :reader defstruct-documentation)
   (%conc-name :initarg :conc-name :reader defstruct-conc-name)
   (%constructors :initarg :constructors :reader defstruct-constructors)
   (%predicates :initarg :predicates :reader defstruct-predicates)
   (%copiers :initarg :copiers :reader defstruct-copiers)
   (%named :initarg :named :reader defstruct-named)
   (%type :initarg :type :reader defstruct-type)
   (%initial-offset :initarg :initial-offset :reader defstruct-initial-offset)
   (%print-object :initarg :print-object :reader defstruct-print-object)
   (%included-structure-name :initarg :included-structure-name :reader defstruct-included-structure-name)
   (%included-slots :initarg :included-slots :reader defstruct-included-slots)
   (%direct-slots :initarg :direct-slots :reader defstruct-direct-slots)))

(defclass slot-description ()
  ((%name :initarg :name :reader slot-name)
   (%accessor-name :initarg :accessor-name :reader slot-accessor-name)
   (%initform :initarg :initform :reader slot-initform)
   (%initform-p :initarg :initform-p :reader slot-initform-p)
   (%type :initarg :type :reader slot-type)
   (%read-only :initarg :read-only :reader slot-read-only)))

(defmethod make-load-form ((object defstruct-description) &optional environment)
  (make-load-form-saving-slots object :environment environment))

(defmethod make-load-form ((object slot-description) &optional environment)
  (make-load-form-saving-slots object :environment environment))
