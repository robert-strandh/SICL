(cl:in-package #:common-lisp-user)

(defpackage #:sicl-compiler
  (:use #:common-lisp)
  (:export #:+first-constant-index+
           #:debug-information
           #:code-object
           #:instructions
           #:frame-maps
           #:callee-saves-register-maps
           #:callee-saves-stack-maps
           #:constants
           #:function-names
           #:hir
           #:compile-ast
           #:tie-code-object))
