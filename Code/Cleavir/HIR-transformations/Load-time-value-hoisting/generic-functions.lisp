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

;;; Return two values: A list of equal keys and a list of equalp keys.  Two
;;; objects o1 and o2 are considered similar if their equal keys have a
;;; common element in the sense of EQUAL, or if their equalp keys have a
;;; common element in the sense of EQUALP.  Clients that want to write
;;; additional methods for this function are encouraged to have a look at
;;; the auxiliary functions EQUAL-REPRESENTATION and EQUALP-REPRESENTATION.
(defgeneric similarity-keys (object system))

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
