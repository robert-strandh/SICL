(cl:in-package #:cleavir-load-time-value-hoisting)

;;; Hoisting takes place after scanning.  During hoisting, all occurring
;;; data are processed by HOIST-DATUM.  The default methods replace all
;;; constant inputs and load time value inputs by lexical locations, and
;;; arrange that each of these lexical locations is properly initialized by
;;; the suitable creation and initialization thunk.

(defmethod hoist-hir ((hir cleavir-ir:instruction) client)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     instruction)
   hir))

(defmethod hoist-datum ((datum cleavir-ir:constant-input) client)
  )
