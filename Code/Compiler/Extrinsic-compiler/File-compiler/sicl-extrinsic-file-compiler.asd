(cl:in-package #:asdf-user)

(defsystem :sicl-extrinsic-file-compiler
  :depends-on (:sicl-reader-simple
	       :cleavir-generate-ast)
  :serial t
  :components
  ((:file "packages")))
