(cl:in-package #:asdf-user)

(defsystem "sicl-package-extrinsic"
  :serial t
  :description "SICL-Specific Package System, extrinsic version"
  :depends-on ("sicl-package-base"
               "parcl-extrinsic")
  :components ((:file "packages-extrinsic")
               . #.*sicl-package-component-designators*))
