(cl:in-package #:cleavir-load-time-value-hoisting)

;;; Return whether OBJECT is an immediate.
(defgeneric immediate-p (object system))

;;; Return the HIR flowchart corresponding to FORM.
(defgeneric hir-from-form (form system))

;;; Similar to CL:MAKE-LOAD-FORM, but instead of receiving an environment
;;; as the second argument, MAKE-LOAD-FORM-USING-CLIENT receives a client
;;; object.
(defgeneric make-load-form-using-client (object system))

;;; Return a list of keys.  Objects with at least one shared key (in the
;;; sense of equalp) are assumed to be similar.
(defgeneric equalp-keys (object system))

;;; Scan all data in HIR with SCAN-DATUM.
(defgeneric scan-hir (hir system))

;;; Ensure that DATUM is either an immediate or has a suitable constructor.
(defgeneric scan-datum (datum system))

;;; Ensure that OBJECT is either an immediate or has a suitable
;;; constructor.
(defgeneric scan-literal-object (object system))

;;; Return a modified version of HIR, where all occurring data have been
;;; suitably processed by HOIST-DATUM.
(defgeneric hoist-toplevel-hir (hir system))

;;; Hoist all data in HIR with HOIST-DATUM.
(defgeneric hoist-hir (hir system))

;;; Modify DATUM in a system dependent way.
(defgeneric hoist-datum (datum system))
