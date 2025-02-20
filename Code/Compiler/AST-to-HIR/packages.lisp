(cl:in-package #:common-lisp-user)

(defpackage #:sicl-ast-to-hir
  (:use #:common-lisp)
  (:local-nicknames (#:ico #:iconoclast)
                    (#:hir #:common-boot-hir)
                    (#:iat #:iconoclast-ast-transformations))
  (:export #:ast-to-hir))
