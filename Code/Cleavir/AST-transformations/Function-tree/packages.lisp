(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-ast-function-tree
  (:use #:common-lisp)
  (:export #:function-tree
           #:traverse
           #:definition
           #:assignments
           #:uses
           #:function-ast
           #:parent
           #:children
           #:child-number
           #:node))
