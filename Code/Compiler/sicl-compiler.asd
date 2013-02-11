(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-compiler
  :depends-on (:sicl-code-utilities)
  :components
  ((:file "packages" :depends-on ())
   (:file "environment" :depends-on ("packages"))
   (:file "phase1" :depends-on ("packages" "environment"))
   (:file "phase2" :depends-on ("packages" "phase1"))
   (:file "word" :depends-on ("packages" "phase1" "phase2"))
   (:file "optimize" :depends-on ("packages" "phase2"))))
