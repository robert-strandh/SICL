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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:EQ.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:eq)) cst env system)
  (cst:db origin (eq-cst arg1-cst arg2-cst) cst
    (declare (ignore eq-cst))
    (cleavir-ast:make-eq-ast
     (convert arg1-cst env system)
     (convert arg2-cst env system)
     :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:TYPEQ.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:typeq)) cst env system)
  (cst:db origin (typeq-cst arg1-cst arg2-cst) cst
    (declare (ignore typeq-cst))
    (cleavir-ast:make-typeq-ast
     (convert arg1-cst env system)
     (cst:raw arg2-cst)
     :origin origin)))
