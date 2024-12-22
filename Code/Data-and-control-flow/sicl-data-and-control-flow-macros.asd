(cl:in-package #:asdf-user)

(defsystem :sicl-data-and-control-flow-macros
  :depends-on (:sicl-data-and-control-flow-support)
  :serial t
  :components
  ((:file "multiple-value-call-defmacro")
   (:file "defconstant-defmacro")
   (:file "throw-defmacro")))
