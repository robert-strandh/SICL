(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-backtrace-inspector
  :depends-on (#:mcclim
               #:sicl-hir-interpreter
               #:clouseau)
  :serial t
  :components
  ((:file "packages")
   (:file "gui")
   (:file "commands")))
