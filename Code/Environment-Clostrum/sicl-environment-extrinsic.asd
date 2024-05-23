(cl:in-package #:asdf-user)

(defsystem "sicl-environment-extrinsic"
  :depends-on ("clostrum"
               "common-boot"
               "sicl-environment-packages-extrinsic")
  :serial t
  :components
  ((:file "variables")
   (:file "environment")
   (:file "fboundp")
   (:file "fdefinition")
   (:file "find-class")
   (:file "macro-function")
   (:file "compiler-macro-function")
   (:file "boundp")
   (:file "symbol-value")
   (:file "find-package")
   (:file "define-constant")))
