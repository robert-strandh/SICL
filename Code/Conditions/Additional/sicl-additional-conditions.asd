(cl:in-package #:asdf-user)

(defsystem #:sicl-additional-conditions
  :depends-on (:cleavir-internationalization
	       :sicl-additional-types)
  :components
  ((:file "packages")
   (:file "conditions" :depends-on ("packages"))
   (:file "condition-reporters-en" :depends-on ("packages" "conditions"))))
