(cl:in-package #:cleavir-cst-to-ast)

(defmethod convert-global-function-reference (cst info global-env system)
  (declare (ignore global-env))
  (let ((source (cst:source cst)))
    (cleavir-ast:make-fdefinition-ast
     (cleavir-ast:make-load-time-value-ast `',(cleavir-env:name info)
                                           t :origin source)
     :attributes (cleavir-env:attributes info)
     :origin source)))

(defmethod convert-function-reference
    (cst (info cleavir-env:global-function-info) env system)
  (convert-global-function-reference
   cst info (cleavir-env:global-environment env) system))

(defmethod convert-function-reference
    (cst (info cleavir-env:local-function-info) env system)
  (declare (ignore env system))
  (cleavir-env:identity info))

(defmethod convert-function-reference
    (cst (info cleavir-env:global-macro-info) env system)
  (error 'function-name-names-global-macro :cst cst))

(defmethod convert-function-reference
    (cst (info cleavir-env:local-macro-info) env system)
  (error 'function-name-names-local-macro :cst cst))

(defmethod convert-function-reference
    (cst (info cleavir-env:special-operator-info) env system)
  (error 'function-name-names-special-operator :cst cst))

;;; These are used by (foo ...) forms.
;;; It's useful to distinguish them. For instance, an implementation
;;; may bind non-fbound symbols to a function that signals an error
;;; of type UNDEFINED-FUNCTION, allowing an fboundp check to be skipped.
;;; Other than the inlining, they by default have the same behavior.

(defmethod convert-called-function-reference (cst info env system)
  (when (not (eq (cleavir-env:inline info) 'cl:notinline))
    (let ((ast (cleavir-env:ast info)))
      (when ast
        (return-from convert-called-function-reference
          ;; The AST must be cloned because hoisting is destructive.
          (cleavir-ast-transformations:clone-ast ast)))))
  (convert-global-function-reference
   cst info (cleavir-env:global-environment env) system))

(defmethod convert-called-function-reference
    (cst (info cleavir-env:local-function-info) env system)
  (declare (ignore env system))
  (cleavir-env:identity info))

(defmethod convert-called-function-reference
    (cst (info cleavir-env:global-macro-info) env system)
  (error 'function-name-names-global-macro :cst cst))

(defmethod convert-called-function-reference
    (cst (info cleavir-env:local-macro-info) env system)
  (error 'function-name-names-local-macro :cst cst))

(defmethod convert-called-function-reference
    (cst (info cleavir-env:special-operator-info) env system)
  (error 'function-name-names-special-operator :cst cst))
