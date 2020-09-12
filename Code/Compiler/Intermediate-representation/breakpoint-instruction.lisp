(cl:in-package #:sicl-ir)

(defclass breakpoint-instruction
    (cleavir-ir:instruction
     cleavir-ir:one-successor-mixin
     cleavir-ir:side-effect-mixin)
  ((%debug-information
    :initform nil
    :initarg :debug-information
    :accessor debug-information)))
