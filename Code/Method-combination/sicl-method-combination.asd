(cl:in-package #:asdf-user)

(defsystem #:sicl-method-combination
  :depends-on (#:cleavir-code-utilities)
  :serial t
  :components
  ((:file "packages")
   (:file "method-combination-template-defclass")
   (:file "define-method-combination-support")))

