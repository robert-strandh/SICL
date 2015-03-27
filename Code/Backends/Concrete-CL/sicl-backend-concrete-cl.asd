(cl:in-package #:asdf-user)

(defsystem #:sicl-backend-concrete-cl
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "system")
   (:file "heap")
   (:file "low")
   (:file "assembler")
   (:file "machine")
   (:file "compiler")))
