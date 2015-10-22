(cl:in-package #:cleavir-cst)

;;;; By REBUILDING a CST C, we mean taking a Common Lisp expression E
;;;; and building a CST from it.  To reconstruct the source
;;;; information, an existing CST D is used.  E may contain
;;;; sub-expressions from D, and in that case the source information
;;;; in D is used for these sub-expressions.
