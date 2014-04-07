(cl:in-package #:common-lisp-user)

(asdf:defsystem :clvm-ast
  :depends-on (:clvm-io)
  :serial t
  :components
  ((:file "packages")
   (:file "general")
   (:file "graphviz-drawing")))
