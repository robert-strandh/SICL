(cl:in-package #:common-lisp-user)

(defpackage #:sicl-compiler
  (:use #:common-lisp)
  (:shadow #:undefined-function)
  (:export #:debug-information
           #:code-object
           #:instructions
           #:constants
           #:ensure-literal
           #:establish-call-site
           #:function-names
           #:ir
           #:hir
           #:compile-ast
           #:tie-code-object
           #:ast-from-file
           #:ast-from-stream
           #:undefined-function
           #:undefined-variable
           #:undefined-block
           #:undefined-tagbody-tag))
