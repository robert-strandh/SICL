(cl:in-package #:asdf-user)

(defsystem #:sicl-compiler-base
  :components
  ((:file "packages")
   (:file "static-environment")
   (:file "breakpoint-instruction")))
