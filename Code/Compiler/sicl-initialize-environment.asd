(cl:in-package #:asdf-user)

(defsystem :sicl-initialize-environment-1
  :depends-on (:sicl-compiler)
  :components
  ((:file "constants")
   (:file "functions")
   (:file "types")))

(defsystem :sicl-initialize-environment
  :depends-on (:sicl-initialize-environment-1)
  :components
  ((:file "compiler-macros")))
