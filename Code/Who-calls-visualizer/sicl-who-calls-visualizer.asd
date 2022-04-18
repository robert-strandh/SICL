(cl:in-package #:asdf-user)

(defsystem #:sicl-who-calls-visualizer
  :depends-on (#:mcclim
               #:sicl-source-tracking)
  :serial t
  :components
  ((:file "mcclim-configuration")
   (:file "packages")
   (:file "gui")
   (:file "commands")))
