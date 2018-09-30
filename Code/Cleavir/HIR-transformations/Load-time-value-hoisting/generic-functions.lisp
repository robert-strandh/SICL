(cl:in-package #:cleavir-load-time-value-hoisting)

;;; Potentially change the class or modify some slots of DATUM, such that
;;; it is easier to process in the remaining steps.
(defgeneric simplify-datum (datum system))

;;; Return the HIR flowchart of a thunk corresponding to FORM.
(defgeneric hir-from-form (form system))

;;; Similar to CL:MAKE-LOAD-FORM, but instead of receiving an environment
;;; as the second argument, MAKE-LOAD-FORM-USING-CLIENT receives a client
;;; object.
(defgeneric make-load-form-using-client (object system))

;;; Return a list of keys.  Objects with at least one shared key (in the
;;; sense of equalp) are assumed to be similar and are coalesced.
(defgeneric equalp-keys (object system))

;;; Call SIMPLIFY-DATUM, followed by SCAN-DATUM on each datum in HIR.
(defgeneric scan-hir (hir system))

;;; Ensure that DATUM has a suitable constructor.
(defgeneric scan-datum (datum system))

;;; Ensure that OBJECT has a suitable constructor.
(defgeneric scan-literal-object (object system))

;;; Return a modified version of HIR, where all occurring data have been
;;; suitably processed by HOIST-DATUM.
(defgeneric hoist-toplevel-hir (hir system))

;;; Hoist all data in HIR with HOIST-DATUM.
(defgeneric hoist-hir (hir system))

;;; Modify DATUM in a system dependent way.
(defgeneric hoist-datum (datum system))
