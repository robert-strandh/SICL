(cl:in-package #:asdf-user)

(defsystem #:sicl-extended-clearcut
  :depends-on (#:clearcut-implementation-s-expression
               #:clearcut-implementation-concrete-syntax-tree)
  :serial t
  :components
  ((:file "packages")
   (:file "extensions")))
               
  
