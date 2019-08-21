(cl:in-package #:common-lisp-user)

(defpackage sicl-hir-transformations
  (:use #:common-lisp)
  (:export #:hoist-fdefinitions
           #:convert-symbol-value
           #:convert-set-symbol-value
           #:eliminate-create-cell-instructions
           #:eliminate-read-cell-instructions
           #:eliminate-write-cell-instructions))
