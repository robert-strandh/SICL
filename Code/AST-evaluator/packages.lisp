(cl:in-package #:common-lisp-user)

(defpackage #:sicl-ast-evaluator
  (:use #:common-lisp)
  (:shadow #:symbol-value #:eval)
  (:local-nicknames (#:env #:sicl-genv)
                    (#:ast #:cleavir-ast))
  (:export))
