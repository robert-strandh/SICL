(cl:in-package #:asdf-user)

(defsystem :sicl-cons-compile-time
  :depends-on (:sicl-cons-package)
  :serial t
  :components
  ((:file "pushnew-support")
   (:file "pushnew-defmacro")
   (:file "push-pop-support")
   (:file "push-pop-defmacro")
   (:file "remf-support")
   (:file "remf-defmacro")
   (:file "with-proper-list-elements-defmacro")
   (:file "with-proper-list-rests-defmacro")
   (:file "with-alist-elements-defmacro")
   (:file "type-proclamations")))
