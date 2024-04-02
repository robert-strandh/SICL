(cl:in-package #:asdf-user)

(defsystem "sicl-typep-subtypep"
  :serial t
  :components
  ((:file "typep-defun")
   (:file "subtypep-defun")))
