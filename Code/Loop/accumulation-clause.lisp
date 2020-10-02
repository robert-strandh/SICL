(cl:in-package #:sicl-loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accumulation clauses

(defclass accumulation-clause (selectable-clause)
  ())

;;; We define three different accumulation CATEGORIES, each identified
;;; by a symbol: LIST, COUNT/SUM, and MAX/MIN.  Accumulation clauses
;;; within a category are compatible in that they can be mixed, even
;;; when they accumulate into the same variable.  This generic
;;; function takes an accumulation clause and returns the category.
(defgeneric accumulation-category (clause))


;;; The methods on ACCUMULATION-VARIABLES call the function INTO-VAR
;;; on the clause in order to obtain the first element of each
;;; accumulation variable descriptor.  For clauses that have
;;; INTO-MIXIN as a superclass, the variable is stored in a slot.
;;; This method defines the default method to be used for all other
;;; accumulation clauses.
(defmethod into-var ((clause accumulation-clause))
  'nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LIST-ACCUMULATION-CLAUSE.
;;;
;;; This class is the superclass of the list accumulation clauses:
;;; COLLECT-CLAUSE, APPEND-CLAUSE, and NCONC-CLAUSE.
;;;

(defclass list-accumulation-clause (accumulation-clause) ())

(defmethod accumulation-category ((clause list-accumulation-clause))
  'list)

;;; The methods on ACCUMULATION-VARIABLES call the function TYPE-SPEC
;;; on the clause in order to obtain the third element of each
;;; accumulation variable descriptor.  For the numeric accumulation
;;; clauses, the type is stored in a slot.  For the list accumulation
;;; clauses, we always want to return the type LIST.
(defmethod type-spec ((clause list-accumulation-clause))
  'list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NUMERIC-ACCUMULATION-CLAUSE.

(defclass numeric-accumulation-clause (accumulation-clause)
  ((%type-spec :initform T :initarg :type-spec :reader type-spec)))

(defclass count/sum-accumulation-clause (numeric-accumulation-clause) ())

(defmethod accumulation-category ((clause count/sum-accumulation-clause))
  'count/sum)

(defclass max/min-accumulation-clause (numeric-accumulation-clause) ())

(defmethod accumulation-category ((clause max/min-accumulation-clause))
  'max/min)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin class for INTO clause variants.

(defclass into-mixin ()
  ((%into-var :initform nil :initarg :into-var :accessor into-var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on ACCUMULATION-VARIABLES, valid for all accumulation
;;; clauses.

(defmethod accumulation-variables ((clause accumulation-clause))
  `((,(into-var clause)
     ,(accumulation-category clause)
     ,(type-spec clause))))
