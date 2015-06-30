(cl:in-package #:asdf-user)

(defsystem :cleavir-internationalization
  :serial t
  :components
  ((:file "packages")
   (:file "locale")
   (:file "date")
   (:file "language")
   (:file "language-english")
   (:file "language-francais")
   (:file "condition")
   (:file "init")))
