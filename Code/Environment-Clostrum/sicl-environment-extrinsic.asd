(cl:in-package #:asdf-user)

(defsystem "sicl-environment-extrinsic"
  :depends-on ("clostrum"
               "common-boot"
               "sicl-environment-packages-extrinsic"
               "sicl-environment-shared"))
