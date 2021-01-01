(cl:in-package #:cleavir-cst-to-ast)

(defmethod convert-global-function-reference (client cst info global-environment)
  (declare (ignore global-environment))
  (let* ((*origin* (cst:source cst))
         (name-ast (cleavir-ast:make-ast 'cleavir-ast:constant-ast
                     :value (trucler:name info))))
    (cleavir-ast:make-ast 'cleavir-ast:fdefinition-ast
      :name-ast name-ast)))

(defmethod convert-function-reference
    (client cst (info trucler:global-function-description) environment)
  (convert-global-function-reference
   client cst info (trucler:global-environment client environment)))

(defmethod convert-function-reference
    (client cst (info trucler:local-function-description) environment)
  (declare (ignore client environment))
  (trucler:identity info))

(defmethod convert-function-reference
    (client cst (info trucler:global-macro-description) environment)
  (error 'function-name-names-global-macro :cst cst))

(defmethod convert-function-reference
    (client cst (info trucler:local-macro-description) environment)
  (error 'function-name-names-local-macro :cst cst))

(defmethod convert-function-reference
    (client cst (info trucler:special-operator-description) environment)
  (error 'function-name-names-special-operator :cst cst))

;;; These are used by (foo ...) forms.
;;; It's useful to distinguish them. For instance, an implementation
;;; may bind non-fbound symbols to a function that signals an error
;;; of type UNDEFINED-FUNCTION, allowing an fboundp check to be skipped.
;;; Other than the inlining, they by default have the same behavior.

(defmethod convert-called-function-reference (client cst info environment)
  (when (not (eq (trucler:inline info) 'cl:notinline))
    (let ((ast (trucler:inline-data info)))
      (when ast
        (return-from convert-called-function-reference
          ;; The AST must be cloned because hoisting is destructive.
          (cleavir-ast-transformations:clone-ast ast)))))
  (convert-global-function-reference
   client cst info (trucler:global-environment client environment)))

(defmethod convert-called-function-reference
    (client cst (info trucler:local-function-description) environment)
  (declare (ignore client environment))
  (trucler:identity info))

(defmethod convert-called-function-reference
    (client cst (info trucler:global-macro-description) environment)
  (error 'function-name-names-global-macro :cst cst))

(defmethod convert-called-function-reference
    (client cst (info trucler:local-macro-description) environment)
  (error 'function-name-names-local-macro :cst cst))

(defmethod convert-called-function-reference
    (client cst (info trucler:special-operator-description) environment)
  (error 'function-name-names-special-operator :cst cst))
