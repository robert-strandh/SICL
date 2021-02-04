(cl:in-package #:common-lisp-user)

(defpackage #:sicl-compiler
  (:use #:common-lisp)
  (:export #:+code-object-index+
           #:+enclose-function-index+
           #:+initialize-closure-function-index+
           #:+cons-function-index+
           #:+nil-index+
           #:+first-constant-index+
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
