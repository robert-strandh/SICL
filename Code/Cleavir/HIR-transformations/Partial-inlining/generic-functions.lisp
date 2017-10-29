(cl:in-package #:cleavir-partial-inlining)

(defgeneric inline-line-instruction
  (call-instruction enter-instruction translation))
