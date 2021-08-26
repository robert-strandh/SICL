(cl:in-package #:asdf-user)

(defsystem :sicl-format
  :depends-on (:acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "generic-functions")
   (:file "format")
   (:file "control-string-compiler")
   (:file "format-define-compiler-macro")
   (:file "conditions")
   (:file "condition-reporters-en")))
