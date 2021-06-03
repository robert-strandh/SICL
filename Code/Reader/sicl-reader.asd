(cl:in-package #:asdf-user)

(defsystem #:sicl-reader
  :depends-on (#:eclector
               #:eclector-concrete-syntax-tree))
