(cl:in-package #:common-lisp-user)

(defpackage #:sicl-ast
  (:use #:common-lisp)
  (:export
   ;; This AST class is the result of converting the
   ;; primop DYNAMIC-ENVIRONMENT.
   #:dynamic-environment-ast
   ;; These two AST classes are the result of converting the primops
   ;; CALLER-STACK-POINTER and CALLER-FRAME-POINTER.
   #:caller-stack-pointer-ast
   #:caller-frame-pointer-ast
   ;; This AST class is the result of converting the
   ;; primop ESTABLISH-STACK-FRAME.
   #:establish-stack-frame-ast
   #:stack-pointer-ast
   #:frame-pointer-ast
   ;; This AST class is the result of converting the primop
   ;; WITH-DYNAMIC-ENVIRONMENT.
   #:with-dynamic-environment-ast
   #:dynamic-environment-ast
   #:body-ast
   ;; This AST class is the result of converting the primop RACK.
   #:rack-ast
   #:standard-object-ast
   ;; This AST class is the result of converting the primop SET-RACK.
   #:set-rack-ast
   #:patch-literal-ast
   #:literal-ast
   #:code-vector-index-ast
   #:literals-vector-index-ast
   #:literal-cell))
