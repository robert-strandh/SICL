(cl:in-package #:asdf-user)

(defsystem #:sicl-source-tracking
  :depends-on (#:trivial-gray-streams
               #:eclector-concrete-syntax-tree)
  :serial t
  :components
  ((:file "packages")
   (:file "source-tracking")
   (:file "stream")))
