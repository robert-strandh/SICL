(cl:in-package #:asdf-user)

(defsystem #:sicl-package
  :serial t
  :description "SICL-Specific Package System"
  :depends-on (#:sicl-package-base)
  :components ((:file "packages")
               . #.*sicl-package-component-designators*))
