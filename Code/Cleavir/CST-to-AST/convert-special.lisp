(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting QUOTE.

(defmethod convert-special
    ((symbol (eql 'quote)) cst env system)
  (cst:db s (quote const) cst
    (declare (ignore quote))
    (convert-constant const env system)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting BLOCK.

(defmethod convert-special
    ((symbol (eql 'block)) cst env system)
  (cst:db origin (block name-cst . body-cst) cst
    (declare (ignore block))
    (let* ((ast (cleavir-ast:make-block-ast nil :origin origin))
           (name (cst:raw name-cst))
	   (new-env (cleavir-env:add-block env name ast)))
      (setf (cleavir-ast:body-ast ast)
	    (process-progn (convert-sequence body-cst new-env system)))
      ast)))
