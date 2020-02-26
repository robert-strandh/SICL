(cl:in-package #:common-lisp-user)

(defpackage #:sicl-compiler
  (:use #:common-lisp)
  (:export #:+code-object-index+
           #:+enclose-function-index+
           #:+cons-function-index+
           #:+nil-index+
           #:+first-constant-index+
           #:debug-information))
