(cl:in-package #:sicl-compiler)

(defclass breakpoint-instruction
    (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ((%debug-information :initarg :debug-information :accessor debug-information)))

