(cl:in-package #:asdf-user)

(defsystem #:sicl-new-boot-backtrace-inspector
  :depends-on (#:mcclim
               #:common-boot-ast-evaluator
               #:sicl-source-tracking
               #:clouseau)
  :serial t
  :components
  ((:file "mcclim-configuration")
   (:file "packages")
   (:file "gui")
   (:file "commands")))
