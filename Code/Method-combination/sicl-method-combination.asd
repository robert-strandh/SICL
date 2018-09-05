(cl:in-package #:asdf-user)

(defsystem #:sicl-method-combination
  :depends-on (#:cleavir-code-utilities
               #:concrete-syntax-tree)
  :serial t
  :components
  ((:file "packages")
   (:file "method-combination-template-defclass")
   (:file "lambda-list-variables")
   (:file "method-group-specifier")
   (:file "method-discriminator")
   (:file "define-method-combination-support")))

