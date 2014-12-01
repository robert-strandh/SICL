(in-package #:common-lisp-user)

(asdf:defsystem cleavir-code-utilities
  :depends-on (:cleavir-internationalization)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "condition-reporters-english")
   (:file "argcount")
   (:file "form")
   (:file "list-structure")
   (:file "declarations")
   (:file "lambda-lists")
   (:file "destructuring")))
