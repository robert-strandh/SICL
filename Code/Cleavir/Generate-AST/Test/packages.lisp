(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-bogus-test-environment
  (:use #:common-lisp)
  (:export #:bogus-environment
	   #:gsm1 #:gsm2
	   #:gm1 #:gm2
	   #:hello1 #:hello2
	   #:hello))

(defpackage #:cleavir-test-minimal-compilation
  (:use #:common-lisp #:cleavir-bogus-test-environment))

(defpackage #:cleavir-test-generate-ast
  (:use #:common-lisp #:cleavir-bogus-test-environment))
