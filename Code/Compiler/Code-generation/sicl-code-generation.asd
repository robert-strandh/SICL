(cl:in-package #:asdf-user)

(defsystem #:sicl-code-generation
  :depends-on (#:sicl-mir-to-lir
               #:cluster)
  :components
  ((:file "packages")
   (:file "linearize")
   (:file "translate-data")
   (:file "translate-instruction")
   (:file "generate-code")))
