(cl:in-package #:cleavir-cst)

;;;; By REBUILDING a CST, we mean taking a Common Lisp expression E
;;;; and building a CST from it.  To reconstruct the source
;;;; information, an existing CST C is used.  E may contain
;;;; sub-expressions from C, and in that case the source information
;;;; in C is used for these sub-expressions.
