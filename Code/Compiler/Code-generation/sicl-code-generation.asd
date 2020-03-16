(cl:in-package #:asdf-user)

(defsystem #:sicl-code-generation
  :depends-on (#:sicl-mir-to-lir
               #:cluster)
  :components
  ((:file "packages")
   (:file "linearize")
   (:file "labels")
   (:file "translate-data")
   (:file "translate-instruction")
   (:file "memory")
   (:file "stack")
   (:file "generate-code")))
