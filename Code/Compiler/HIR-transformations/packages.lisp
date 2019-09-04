(cl:in-package #:common-lisp-user)

(defpackage sicl-hir-transformations
  (:use #:common-lisp)
  (:export #:hoist-fdefinitions
           #:find-function-cell-instruction
           #:convert-symbol-value
           #:convert-set-symbol-value
           #:eliminate-create-cell-instructions
           #:eliminate-fetch-instructions
           #:eliminate-read-cell-instructions
           #:eliminate-write-cell-instructions
           #:constants
           #:top-level-enter-instruction
           #:hoist-constant-inputs))
