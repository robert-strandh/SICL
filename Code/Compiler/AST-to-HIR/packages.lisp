(cl:in-package #:common-lisp-user)

(defpackage #:sicl-ast-to-hir
  (:use #:common-lisp)
  (:local-nicknames (#:ico #:iconoclast)
                    (#:hir #:sicl-hir)
                    (#:iat #:iconoclast-ast-transformations))
  (:export #:ast-to-hir))
