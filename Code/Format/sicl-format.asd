(cl:in-package #:asdf-user)

(defsystem :sicl-format
  :depends-on (:acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "condition-reporters-en")
   (:file "format")
   (:file "control-string-compiler")
   (:file "format-define-compiler-macro")))
