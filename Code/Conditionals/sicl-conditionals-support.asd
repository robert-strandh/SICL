;;;; This file is part of the conditionals module of the SICL project.
;;;; See the file SICL.text for a description of the project. 
;;;; See the file conditionals.text for a description of the module.

(cl:in-package #:asdf-user)

(defsystem :sicl-conditionals-support
  :depends-on (:acclimation
	       :cleavir-code-utilities)
  :serial t
  :components
  ((:file "packages")
   (:file "support")
   (:file "conditions")
   (:file "condition-reporters-en")
   (:file "docstrings-en")))
