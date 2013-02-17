(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-compiler
  :depends-on (:sicl-code-utilities)
  :components
  ((:file "packages" :depends-on ())
   (:file "environment" :depends-on ("packages"))
   (:file "abstract-syntax-tree" :depends-on ("packages" "environment"))
   (:file "mir" :depends-on ("packages"))
   (:file "phase1"
    :depends-on ("packages" "environment" "abstract-syntax-tree"))
   (:file "phase2" :depends-on ("packages" "phase1" "mir"))
   (:file "optimize" :depends-on ("packages" "phase2"))))
