(cl:in-package #:sicl-ir)

(defclass patch-literal-instruction
    (cleavir-ir:instruction
     cleavir-ir:one-successor-mixin
     cleavir-ir:side-effect-mixin)
  (;; This slot holds a CONS cell that is shared with the LOAD-LITERAL
   ;; instruction.
   (%literal-cell :initarg :literal-cell :reader literal-cell)
   ;; This slot holds a CONS cell that (in its CAR) holds an index
   ;; into the code vector where the object created by the load-time
   ;; action must be stored.
   (%code-vector-index-cell
    :initform (list nil)
    :reader code-vector-index-cell)
   ;; This slot holds a CONS cell that (in its CAR) holds an index
   ;; into the literals vector where the object created by the
   ;; load-time action must be stored.
   (%literals-vector-index-cell
    :initform (list nil)
    :reader literals-vector-index-cell)))
