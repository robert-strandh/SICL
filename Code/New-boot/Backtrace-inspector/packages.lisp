(cl:in-package #:common-lisp-user)

(defpackage #:sicl-new-boot-backtrace-inspector
  (:use #:common-lisp)
  (:local-nicknames (#:cb #:common-boot)
                    (#:cbe #:common-boot-hir-evaluator))
  (:shadow #:inspect #:inspector)
  (:export #:inspect))
