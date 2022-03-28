(cl:in-package #:asdf-user)

(defsystem #:sicl-compiler-base
  :depends-on (#:sicl-primop
               #:sicl-ast
               #:sicl-ir)
  :components
  ((:file "packages")
   (:file "ensure-literal")
   (:file "conditions")))
