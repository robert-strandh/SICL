(cl:in-package #:sicl-extrinsic-hir-interpreter)

(defclass enter-instruction (cleavir-ir:enter-instruction)
  ((%owned-variables :initarg :owned-variables :reader owned-variables)))
