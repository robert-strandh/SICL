(cl:in-package #:common-lisp-user)

(defpackage #:sicl-ast-to-hir-test
  (:use #:common-lisp)
  (:local-nicknames (#:cb #:common-boot)
                    (#:cmd #:common-macro-definitions))
  (:export))
