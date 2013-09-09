(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-boot
  :depends-on (:sicl-compiler
	       :sicl-conditionals)
  :components
  ((:file "initialize-environment")))
