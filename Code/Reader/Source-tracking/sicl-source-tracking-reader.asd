(cl:in-package #:asdf-user)

(defsystem sicl-source-tracking-reader
  :depends-on (:sicl-reader-simple
	       :cleavir-cst)
  :serial t
  :components
  ((:file "read")))
