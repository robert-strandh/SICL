(cl:in-package #:asdf-user)

(defsystem #:maclina-backtrace-inspector
  :depends-on (#:mcclim
               #:dissect
               #:sicl-source-tracking
               #:clouseau)
  :serial t
  :components
  ((:file "mcclim-configuration")
   (:file "packages")
   (:file "gui")
   (:file "commands")))
