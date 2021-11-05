(cl:in-package #:sicl-cst-to-ast)

;;;; FIXME: Since the name of the symbol is known, there should be a
;;;; LOAD-TIME-VALUE that accesses the variable cell, so that we don't
;;;; have to do a hash-table lookup at run-time.  So, instead of
;;;; calling CL:SYMBOL-VALUE, we should call SICL-SYMBOL:VARIABLE-CELL
;;;; at load time, and SICL-RUN-TIME:SYMBOL-VALUE at run time, passing
;;;; it the variable cell.

(defmethod cleavir-cst-to-ast:convert-special-variable
    ((client sicl-client:sicl) cst info global-env)
  (make-instance 'cleavir-ast:named-call-ast
    :callee-name 'symbol-value
    :argument-asts
    (list
     (cleavir-ast:make-ast 'cleavir-ast:literal-ast
       :value (trucler:name info)))))

(defmethod cleavir-cst-to-ast:convert-setq-special-variable
    ((client sicl-client:sicl) var-cst form-ast info global-env)
  (make-instance 'cleavir-ast:named-call-ast
    :callee-name '(setf symbol-value)
    :argument-asts
    (list
     form-ast
     (cleavir-ast:make-ast 'cleavir-ast:literal-ast
       :value (trucler:name info)))))
