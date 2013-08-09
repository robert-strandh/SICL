(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-initialize-environment-1
  :depends-on (:sicl-compiler)
  :components
  ((:file "macros")
   (:file "constants")
   (:file "functions")
   (:file "types")))

(asdf:defsystem :sicl-initialize-environment
  :depends-on (:sicl-initialize-environment-1)
  :components
  ((:file "compiler-macros")))
