(cl:in-package #:asdf-user)

;;;; This system implements an adaptation of Kildall's algorithm so
;;;; that instead of computing information about common subexpressions
;;;; it does "value numbering".
;;;;
;;;; In this version of value numbering, we are only concerned with
;;;; lexical locations and assignment instructions.  Any other
;;;; operation is considered unique in that it creates a fresh
;;;; symbolic value.  In other words, we do not attempt to identify
;;;; symbolic values of two similar operations with identical symbolic
;;;; values as arguments.
;;;;
;;;; The information computed by this system will allow us to
;;;; determine whether the value of some lexical location L1 at some
;;;; program point P1 is always the same as the value of some lexical
;;;; location L2 at some program point P2 as a result of a chain of
;;;; assignment instructions from P1 to P2.

(defsystem :cleavir-simple-value-numbering
  :depends-on (:cleavir-meter
	       :cleavir-hir-transformations
	       :cleavir-liveness)
  :serial t
  :components
  ((:file "packages")
   (:file "meter")
   (:file "simple-value-numbering")))
