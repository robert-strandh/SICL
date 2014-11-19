(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-internationalization
  :serial t
  :components
  ((:file "packages")
   (:file "locale")
   (:file "date")
   (:file "language")
   (:file "language-english")
   (:file "condition")
   (:file "init")))
