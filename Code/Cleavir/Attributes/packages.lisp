(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-attributes
  (:use #:common-lisp)
  (:export #:make-attributes #:default-attributes
           #:has-boolean-attribute-p))
