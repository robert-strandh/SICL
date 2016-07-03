(cl:in-package #:asdf-user)

(defsystem sicl-source-tracking-reader
  :depends-on (:sicl-reader-simple
	       :cleavir-cst
	       :trivial-gray-streams)
  :serial t
  :components
  ((:file "packages")
   (:file "read")
   (:file "stream")))
