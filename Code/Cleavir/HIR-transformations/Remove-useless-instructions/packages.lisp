(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-remove-useless-instructions
  (:use #:common-lisp)
  (:export #:remove-useless-instructions
	   #:instruction-may-be-removed-p))
