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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:CAR.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:car)) cst env system)
  (cst:db origin (car-cst arg-cst) cst
    (declare (ignore car-cst))
    (cleavir-ast:make-car-ast (convert arg-cst env system)
                              :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:CDR.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:cdr)) cst env system)
  (cst:db origin (cdr-cst arg-cst) cst
    (declare (ignore cdr-cst))
    (cleavir-ast:make-cdr-ast (convert arg-cst env system)
                              :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:RPLACA.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:rplaca)) cst env system)
  (cst:db origin (rplaca-cst arg1-cst arg2-cst) cst
    (declare (ignore rplaca-cst))
    (cleavir-ast:make-rplaca-ast (convert arg1-cst env system)
                                 (convert arg2-cst env system)
                                 :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:RPLACD.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:rplacd)) cst env system)
  (cst:db origin (rplacd-cst arg1-cst arg2-cst) cst
    (declare (ignore rplacd-cst))
    (cleavir-ast:make-rplacd-ast (convert arg1-cst env system)
                                 (convert arg2-cst env system)
                                 :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-ADD.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-add)) cst env system)
  (cst:db origin (add-cst arg1-cst arg2-cst variable-cst) cst
    (declare (ignore add-cst))
    (cleavir-ast:make-fixnum-add-ast (convert arg1-cst env system)
                                     (convert arg2-cst env system)
                                     (convert variable-cst env system)
                                     :origin origin)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-SUB.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-sub)) cst env system)
  (cst:db origin (sub-cst arg1-cst arg2-cst variable-cst) cst
    (declare (ignore sub-cst))
    (cleavir-ast:make-fixnum-sub-ast (convert arg1-cst env system)
                                     (convert arg2-cst env system)
                                     (convert variable-cst env system)
                                     :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-LESS.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-less)) cst env system)
  (cst:db origin (less-cst arg1-cst arg2-cst) cst
    (declare (ignore less-cst))
    (make-instance 'cleavir-ast:fixnum-less-ast
      :arg1-ast (convert arg1-cst env system)
      :arg2-ast (convert arg2-cst env system)
      :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-NOT-GREATER.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-not-greater)) cst env system)
  (cst:db origin (not-greater-cst arg1-cst arg2-cst) cst
    (declare (ignore not-greater-cst))
    (make-instance 'cleavir-ast:fixnum-not-greater-ast
      :arg1-ast (convert arg1-cst env system)
      :arg2-ast (convert arg2-cst env system)
      :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-GREATER.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-greater)) cst env system)
  (cst:db origin (greater-cst arg1-cst arg2-cst) cst
    (declare (ignore greater-cst))
    (make-instance 'cleavir-ast:fixnum-greater-ast
      :arg1-ast (convert arg1-cst env system)
      :arg2-ast (convert arg2-cst env system)
      :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CLEAVIR-PRIMOP:FIXNUM-NOT-LESS.

(defmethod convert-special
    ((symbol (eql 'cleavir-primop:fixnum-not-less)) cst env system)
  (cst:db origin (not-less-cst arg1-cst arg2-cst) cst
    (declare (ignore not-less-cst))
    (make-instance 'cleavir-ast:fixnum-not-less-ast
      :arg1-ast (convert arg1-cst env system)
      :arg2-ast (convert arg2-cst env system)
      :origin origin)))
