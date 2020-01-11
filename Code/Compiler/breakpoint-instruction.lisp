(cl:in-package #:sicl-ast-to-hir)

(defclass breakpoint-instruction
    (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ((%debug-information :initarg :debug-information :accessor debug-information)))

