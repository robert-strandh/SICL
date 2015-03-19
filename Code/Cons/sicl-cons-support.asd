(cl:in-package #:asdf-user)

(defsystem :sicl-cons-support
  :depends-on (:sicl-cons-package)
  :serial t
  :components
  ((:file "make-bindings-defun")))
