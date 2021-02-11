(cl:in-package #:common-lisp-user)

(defpackage sicl-hir-transformations
  (:use #:common-lisp)
  (:export #:hoist-fdefinitions
           #:name
           #:eliminate-create-cell-instructions
           #:eliminate-fetch-instructions
           #:eliminate-read-cell-instructions
           #:eliminate-write-cell-instructions
           #:constants
           #:function-names
           #:top-level-enter-instruction
           #:process-constant-inputs
           #:eliminate-fixed-to-multiple-instructions
           #:eliminate-multiple-to-fixed-instructions
           #:preprocess-multiple-value-call-instructions
           #:eliminate-append-values-instructions))
