(cl:in-package #:asdf-user)

;;;; A CONCRETE SYNTAX TREE (or CST for short) is a representation of
;;;; a Common Lisp expression such that the source location of each
;;;; object in the expression can be associated with that object.
;;;;
;;;; This system contains functions for manipulating such
;;;; representations in various ways.

(defsystem :cleavir-cst
  :serial t
  :components
  ((:file "packages")
   (:file "concrete-syntax-tree")))
