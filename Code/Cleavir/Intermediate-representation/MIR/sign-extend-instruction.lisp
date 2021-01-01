(cl:in-package #:cleavir-ir)

(defclass sign-extend-instruction (instruction one-successor-mixin)
  ())

(normalize-arguments
 sign-extend-instruction
 (input)
 (output)
 (successor))
