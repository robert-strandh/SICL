(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-boot
  :depends-on (:sicl-compiler
	       :sicl-conditionals
	       :sicl-arithmetic
	       :sicl-conditions
	       :sicl-type)
  :components
  ((:file "initialize-environment")))
