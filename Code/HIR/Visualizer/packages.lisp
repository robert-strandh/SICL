(cl:in-package #:common-lisp-user)

(defpackage #:sicl-hir-visualizer
  (:use #:common-lisp)
  (:local-nicknames (#:ir #:sicl-hir))
  (:export #:visualize))
