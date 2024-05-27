(cl:in-package #:asdf-user)

(defsystem "sicl-environment-shared"
  :serial t
  :components
  ((:file "variables")
   (:file "fboundp")
   (:file "fdefinition")
   (:file "find-class")
   ;; (:file "environment")
   (:file "macro-function")
   (:file "compiler-macro-function")
   (:file "boundp")
   ;; (:file "symbol-value")
   ;; (:file "find-package")
   (:file "define-constant")
   ))
