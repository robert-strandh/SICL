(cl:in-package #:asdf-user)

(defsystem #:sicl-conditions-compile-time
  :depends-on (#:sicl-conditions-support)
  :serial t
  :components
  ((:file "assert-defmacro")
   (:file "check-type-defmacro")
   (:file "handler-clusters-defvar")
   (:file "handler-bind-defmacro")
   (:file "handler-utilities")
   (:file "handler-case-defmacro")
   (:file "ignore-errors-defmacro")
   (:file "define-condition-support")
   (:file "define-condition-defmacro")
   (:file "restart-clusters-defvar")
   (:file "restarts-utilities")
   (:file "restart-bind-defmacro")
   (:file "restart-case-defmacro")
   (:file "with-condition-restarts-defmacro")
   (:file "with-simple-restart-defmacro")
   (:file "with-store-value-restart-defmacro")))
