(cl:in-package #:asdf-user)

(defsystem :sicl-format
  :depends-on (:acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "generic-functions")
   (:file "directive")
   (:file "parse-control-string")
   (:file "split-control-string")
   (:file "structure-items")
   (:file "control-string-compiler")
   (:file "format")
   (:file "format-define-compiler-macro")
   (:file "conditions")
   (:file "condition-reporters-en")))
