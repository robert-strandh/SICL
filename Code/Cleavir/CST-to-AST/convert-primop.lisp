(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:AST.
;;;
;;; This allows ASTs produced by other means to be inserted into
;;; code which is then converted again.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:ast)) cst env system)
  (declare (ignore env system))
  (cst:db origin (primop ast) cst
    (declare (ignore primop))
    ast))
