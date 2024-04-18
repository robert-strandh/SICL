(cl:in-package #:asdf-user)

(defsystem "sicl-environment-intrinsic"
  :depends-on ("clostrum"
               "sicl-run-time"
               "sicl-environment-packages-intrinsic"
               "sicl-environment-shared"))
