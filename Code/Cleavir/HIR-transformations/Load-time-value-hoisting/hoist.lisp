(cl:in-package #:cleavir-load-time-value-hoisting)

;;; Hoisting takes place after scanning.  During hoisting, all occurring
;;; data are processed by HOIST-DATUM.  The default methods replace all
;;; constant inputs and load time value inputs by lexical locations, and
;;; arrange that each of these lexical locations is properly initialized by
;;; the suitable creation and initialization thunk.

(defvar *data-table*)

(defvar *prologue*)

(defmacro lexical-location (key)
  `(gethash ,key *data-table*))

(defmethod hoist-toplevel-hir ((hir cleavir-ir:instruction) client)
  (let ((*data-table (make-hash-table :test #'eq))
        (*prologue* nil))
    (hoist-hir hir client)))

(defmethod hoist-hir ((hir cleavir-ir:instruction) client)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (loop for datum in (cleavir-ir:inputs instruction) do
       (hoist-datum datum client))
     instruction)
   hir))

(defmethod hoist-datum ((constant-input cleavir-ir:constant-input) client)
  (let ((constructor
          (constructor (cleavir-ir:value constant-input)))
        (lexical-location
          (change-class datum 'lexical-location :name (gensym))))
    ))
