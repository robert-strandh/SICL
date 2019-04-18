(cl:in-package #:cleavir-cst-to-ast)

(defmethod convert-global-function-reference (cst info global-env client)
  (declare (ignore global-env))
  (make-instance 'cleavir-ast:fdefinition-ast
   :name (cleavir-env:name info)))

(defmethod convert-function-reference
    (cst (info cleavir-env:global-function-info) env client)
  (convert-global-function-reference
   cst info (cleavir-env:global-environment env) client))

(defmethod convert-function-reference
    (cst (info cleavir-env:local-function-info) env client)
  (declare (ignore env client))
  (cleavir-env:identity info))

(defmethod convert-function-reference
    (cst (info cleavir-env:global-macro-info) env client)
  (error 'function-name-names-global-macro
         :expr (cleavir-env:name info)))

(defmethod convert-function-reference
    (cst (info cleavir-env:local-macro-info) env client)
  (error 'function-name-names-local-macro
         :expr (cleavir-env:name info)))

(defmethod convert-function-reference
    (cst (info cleavir-env:special-operator-info) env client)
  (error 'function-name-names-special-operator
         :expr (cleavir-env:name info)))

;;; These are used by (foo ...) forms.
;;; It's useful to distinguish them. For instance, an implementation
;;; may bind non-fbound symbols to a function that signals an error
;;; of type UNDEFINED-FUNCTION, allowing an fboundp check to be skipped.
;;; Other than the inlining, they by default have the same behavior.

(defmethod convert-called-function-reference (cst info env client)
  (when (not (eq (cleavir-env:inline info) 'cl:notinline))
    (let ((ast (cleavir-env:ast info)))
      (when ast
        (return-from convert-called-function-reference
          ;; The AST must be cloned because hoisting is destructive.
          (cleavir-ast-transformations:clone-ast ast)))))
  (convert-global-function-reference
   cst info (cleavir-env:global-environment env) client))

(defmethod convert-called-function-reference
    (cst (info cleavir-env:local-function-info) env client)
  (declare (ignore env client))
  (cleavir-env:identity info))

(defmethod convert-called-function-reference
    (cst (info cleavir-env:global-macro-info) env client)
  (error 'function-name-names-global-macro
         :expr (cleavir-env:name info)))

(defmethod convert-called-function-reference
    (cst (info cleavir-env:local-macro-info) env client)
  (error 'function-name-names-local-macro
         :expr (cleavir-env:name info)))

(defmethod convert-called-function-reference
    (cst (info cleavir-env:special-operator-info) env client)
  (error 'function-name-names-special-operator
         :expr (cleavir-env:name info)))
