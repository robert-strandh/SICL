(cl:in-package #:common-lisp-user)

(defpackage #:sicl-ast-evaluator
  (:use #:common-lisp)
  (:shadow #:eval)
  (:local-nicknames (#:env #:sicl-environment)
                    (#:client #:sicl-client)
                    (#:ast #:cleavir-ast))
  (:export #:translate-top-level-ast
           #:translate-ast
           #:eval-ast))
