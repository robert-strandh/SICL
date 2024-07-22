(cl:in-package #:asdf-user)

(defsystem #:sicl-compilation-environment-information
  :depends-on (#:trucler-reference)
  :serial t
  :components
  ((:file "packages")
   (:file "generic-function-description")))

