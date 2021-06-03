(cl:in-package #:asdf-user)

(defsystem #:sicl-source-tracking
  :depends-on (#:trivial-gray-streams
               #:sicl-reader
               #:cleavir-io)
  :serial t
  :components
  ((:file "packages")
   (:file "source-tracking")
   (:file "stream")))
