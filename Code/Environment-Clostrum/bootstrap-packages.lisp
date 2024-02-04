(cl:in-package #:common-lisp-user)

(defpackage #:sicl-environment
  (:export #:*environment*
           #:*client*
           #:fdefinition
           #:find-class
           #:type-expander))
