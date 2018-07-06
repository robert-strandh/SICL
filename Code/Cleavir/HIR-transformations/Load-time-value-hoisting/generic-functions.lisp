(cl:in-package #:cleavir-load-time-value-hoisting)

(defvar *compilation-environment*)

;;; Create a new constructor and scan its creation and initialization form.
(defgeneric make-constructor (object system))

;;; The first argument of a MAKE-LOAD-FORM call.
(defgeneric creation-form (constructor))

;;; The second argument of a MAKE-LOAD-FORM call.
(defgeneric initialization-form (constructor))

;;; The HIR flowchart of a thunk returning a fresh object.
(defgeneric creation-thunk (constructor))

;;; The HIR flowchart of a thunk that returns no values.
(defgeneric initialization-thunk (constructor))

;;; Return the HIR flowchart corresponding to FORM.
(defgeneric compile-form (form system))

;;; Scan all data in HIR with SCAN-DATUM.
(defgeneric scan-hir (hir system))

;;; Ensure that DATUM is either an immediate or has a suitable constructor.
(defgeneric scan-datum (datum system))

;;; Ensure that OBJECT is either an immediate or has a suitable
;;; constructor.
(defgeneric scan-literal-object (object system))

;;; Whether the creation form of CONSTRUCTOR is currently being scanned.
(defgeneric creation-form-finalized-p (constructor))

;;; Return a list of keys.  Two objects with at least one EQUALP key are
;;; assumed to be similar.
(defgeneric equalp-keys (object system))

;;; Return a modified version of HIR, where all occurring data have been
;;; suitably processed by HOIST-DATUM.
(defgeneric hoist-toplevel-hir (hir system))

;;; Hoist all data in HIR with HOIST-DATUM.
(defgeneric hoist-hir (hir system))

;;; Modify DATUM in a system dependent way.
(defgeneric hoist-datum (datum system))
