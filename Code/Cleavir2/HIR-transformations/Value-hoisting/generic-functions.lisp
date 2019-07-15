(cl:in-package #:cleavir-value-hoisting)

;;; Return the HIR flowchart of a thunk corresponding to FORM.
(defgeneric hir-from-form (client form environment))

;;; Similar to CL:MAKE-LOAD-FORM, but with an additional client argument.
(defgeneric make-load-form-using-client (client object environment))

;;; Returns up to two values, where the first value is a list of equal
;;; keys, and where the second value is a list of equalp keys.
;;
;;; Two objects o1 and o2 are considered similar if their equal keys have a
;;; common element in the sense of EQUAL, or if their equalp keys have a
;;; common element in the sense of EQUALP.  Clients that want to write
;;; additional methods for this function are encouraged to have a look at
;;; the auxiliary functions EQUAL-REPRESENTATION and EQUALP-REPRESENTATION.
(defgeneric similarity-keys (client object))

;;; Call SCAN-DATUM on each datum in HIR.
(defgeneric scan-hir (client hir))

;;; Ensure that DATUM has a suitable constructor.
(defgeneric scan-datum (client datum))

;;; Ensure that OBJECT has a suitable constructor.
(defgeneric scan-literal-object (client object))

;;; Return a modified version of HIR, where all occurring data have been
;;; suitably processed by HOIST-DATUM.
(defgeneric hoist-toplevel-hir (client hir))

;;; Hoist all data in HIR with HOIST-DATUM.
(defgeneric hoist-hir (client hir))

;;; Hoist DATUM in a client-specific way.
(defgeneric hoist-datum (client datum))
