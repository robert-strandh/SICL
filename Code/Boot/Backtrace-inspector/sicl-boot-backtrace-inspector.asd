(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-backtrace-inspector
  :depends-on (#:mcclim
               #:sicl-source-tracking
               #:sicl-hir-evaluator
               #:clouseau)
  :serial t
  :components
  ((:file "mcclim-configuration")
   (:file "packages")
   (:file "gui")
   (:file "commands")))
