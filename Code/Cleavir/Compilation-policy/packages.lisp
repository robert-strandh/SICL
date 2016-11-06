(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-compilation-policy
  (:use #:common-lisp)
  (:nicknames #:cleavir-policy)
  (:export #:policy-value)
  (:export #:policy-qualities #:define-cleavir-policy-quality)
  (:export #:optimize-value #:normalize-optimize)
  (:export #:compute-policy #:compute-policy-quality))
