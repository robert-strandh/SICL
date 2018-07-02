(cl:in-package #:cleavir-load-time-value-hoisting)

(defvar *compilation-environment*)

;;; Return the HIR flowchart corresponding to FORM.
(defgeneric compile-form (form client))

;;; Scan all data in HIR with SCAN-DATUM.
(defgeneric scan-hir (hir client))

;;; Scan all literal objects in DATUM with SCAN-LITERAL-OBJECT.
(defgeneric scan-datum (datum client))

;;; Ensure that the literal object has a suitable constructor.  Return that
;;; constructor.
(defgeneric scan-literal-object (constant client))

;;; Hoist all data in HIR with HOIST-DATUM.
(defgeneric hoist-hir (hir client))

;;; Modify DATUM in a client dependent way.
(defgeneric hoist-datum (datum client))

;;; Whether the creation form of CONSTRUCTOR is currently being scanned.
(defgeneric scanning-creation-form-p (constructor))

;;; The first argument of a MAKE-LOAD-FORM call.
(defgeneric creation-form (constructor))

;;; The second argument of a MAKE-LOAD-FORM call.
(defgeneric initialization-form (constructor))

;;; The HIR flowchart of a thunk returning a fresh object.
(defgeneric creation-thunk (constructor))

;;; The HIR flowchart of a thunk that returns no values.
(defgeneric initialization-thunk (constructor))

;;; An object serving as a reference of what to construct.
(defgeneric prototype (constructor))
