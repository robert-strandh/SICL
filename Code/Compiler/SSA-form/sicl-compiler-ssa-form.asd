(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-compiler-ssa-form
  :depends-on (:sicl-compiler-utilities
	       :sicl-compiler-dominance)
  :components
  ((:file "packages")
   (:file "ssa-form" :depends-on ("packages"))))

