(cl:in-package #:asdf-user)

(defsystem :sicl-environment-support
  :serial t
  :components
  ((:file "packages")
   (:file "defun-support")))
