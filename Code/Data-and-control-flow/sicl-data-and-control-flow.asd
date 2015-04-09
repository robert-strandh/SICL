(cl:in-package #:asdf-user)

(defsystem :sicl-data-and-control-flow
  :depends-on (:sicl-data-and-control-flow-support)
  :serial t
  :components
  ((:file "setf")
   (:file "multiple-value-list")
   (:file "nth-value")
   (:file "get-setf-expansion")
   (:file "return-defmacro")
   (:file "prog1-prog2-defmacro")
   (:file "prog-progstar-defmacro")
   (:file "psetq-defmacro")
   (:file "psetf-defmacro")
   (:file "rotatef-defmacro")
   (:file "destructuring-bind-defmacro")
   (:file "shiftf-defmacro")))
