(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-compilation-policy
  (:use #:common-lisp)
  (:nicknames #:cleavir-policy)
  (:export #:policy
	   #:infer-types-p))
