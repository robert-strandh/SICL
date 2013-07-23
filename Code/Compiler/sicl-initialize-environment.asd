(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-initialize-environment
  :depends-on (:sicl-compiler)
  :components
  ((:file "macros")
   (:file "constants")
   (:file "functions")
   (:file "types")))

