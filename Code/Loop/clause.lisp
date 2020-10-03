(cl:in-package #:sicl-loop)

;;;; The terminology used here is that of the BNF grammar in the
;;;; dictionary description of the loop macro in the HyperSpec.  It is
;;;; not the same as the terminology used in the section 6.1.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Common classes.

;;; The base class of all clauses.
(defclass clause () ())

;;; Mixin for clauses that accept `AND'.
(defclass subclauses-mixin ()
  ((%subclauses :initarg :subclauses :reader subclauses)))

;;; Mixin for clauses and subclauses that take
;;; a VAR-SPEC and a TYPE-SPEC.
(defclass var-and-type-spec-mixin ()
  ((%var-spec :initarg :var-spec :accessor var-spec)
   (%type-spec :initarg :type-spec :accessor type-spec)))

;;; Mixin for clauses that take a list of compound forms.
(defclass compound-forms-mixin ()
  ((%forms :initarg :forms :reader forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin for clauses that make the loop return a value.

(defclass loop-return-clause-mixin () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin for clauses that has an implicit IT argument.

(defclass it-mixin () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin for clauses that has an explicit form argument.

(defclass form-mixin ()
  ((%form :initarg :form :reader form)))
