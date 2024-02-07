(cl:in-package #:asdf-user)

(defsystem "sicl-environment-extrinsic"
  :depends-on ("clostrum"
               "sicl-run-time"
               "sicl-environment-packages-intrinsic"
               "sicl-environment-shared"))
