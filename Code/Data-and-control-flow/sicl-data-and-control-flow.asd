(cl:in-package #:asdf-user)

(defsystem :sicl-data-and-control-flow
  :depends-on (:sicl-data-and-control-flow-support)
  :serial t
  :components
  ((:file "identity-defun")
   (:file "constantly-defun")
   (:file "not-defun")
   (:file "eq-defun")))
