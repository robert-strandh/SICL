(cl:in-package #:asdf-user)

(defsystem "sicl-cst-from-expression-with-source-info"
  :depends-on ("inravina-extrinsic"
               "trivial-gray-streams"
               "concrete-syntax-tree"
               "sicl-source-tracking")
  :serial t
  :components
  ((:file "packages")
   (:file "stream")
   (:file "cst-from-expression")))
