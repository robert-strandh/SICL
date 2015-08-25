(cl:in-package #:asdf-user)

;;;; This system implements an adaptation of Kildall's algorithm so
;;;; that instead of computing information about common subexpressions
;;;; it does "value numbering".

(defsystem :cleavir-simple-value-numbering
  :depends-on (:cleavir-meter
	       :cleavir-hir-transformations
	       :cleavir-liveness)
  :serial t
  :components
  ((:file "packages")
   (:file "meter")
   (:file "simple-value-numbering")))
