(cl:in-package #:asdf-user)

(defsystem :sicl-cons-support
  :depends-on (:sicl-cons-package :acclimation)
  :serial t
  :components
  ((:file "make-bindings-defun")
   (:file "conditions")
   (:file "pushnew-support")
   (:file "remf-support")
   (:file "push-pop-support")))
