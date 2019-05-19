(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-remove-useless-instructions
  (:use #:common-lisp)
  (:export #:remove-useless-instructions
           #:remove-useless-instructions-from
           #:remove-useless-instructions-with-worklist
	   #:instruction-may-be-removed-p))
