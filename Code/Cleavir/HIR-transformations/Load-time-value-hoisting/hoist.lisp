(cl:in-package #:cleavir-load-time-value-hoisting)

;;; Hoisting takes place after scanning.  During hoisting, all occurring
;;; data are processed by HOIST-DATUM.  The default methods replace all
;;; constant inputs and load time value inputs by lexical locations, and
;;; arrange that each of these lexical locations is properly initialized by
;;; the suitable creation and initialization thunk.  This sequence of
;;; thunks is placed right after the enter instruction that is passed as a
;;; first argument to HOIST-TOPLEVEL-HIR.

(defvar *initial-instruction*)

(defmacro lexical-location (key)
  `(gethash ,key *data-table*))

(defmethod hoist-toplevel-hir ((hir cleavir-ir:enter-instruction) system)
  (let ((*initial-instruction* hir))
    (hoist-hir hir system)))

(defmethod hoist-hir ((hir cleavir-ir:instruction) system)
  (cleavir-ir:map-instructions
   (lambda (instruction)
     (loop for datum in (cleavir-ir:inputs instruction)
           unless (immediate-p datum system) do
       (hoist-datum datum system))
     instruction)
   hir))

(defmethod hoist-datum ((constant-input cleavir-ir:constant-input) system)
  (let ((constructor
          (constructor (cleavir-ir:value constant-input)))
        (lexical-location
          (change-class constant-input 'lexical-location :name (gensym)))
        (values-location
          (cleavir-ir:make-values-location)))
    ))
