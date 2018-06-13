(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-basic-blocks
  (:use #:common-lisp)
  (:export #:basic-block
           #:first-instruction
           #:last-instruction
           #:predecessors
           #:successors
           #:owner
           #:basic-blocks
           #:instruction-basic-blocks
           #:map-basic-block-instructions
           #:containing-basic-block))
