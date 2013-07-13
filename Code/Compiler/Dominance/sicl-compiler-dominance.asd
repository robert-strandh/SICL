(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-compiler-dominance
  :depends-on (:sicl-compiler-utilities)
  :components
  ((:file "packages" :depends-on ())
   (:file "dominance" :depends-on ("packages"))))
