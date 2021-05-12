(cl:in-package #:sicl-compiler)

(defun check-every-location-is-defined (mir)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (dolist (input (cleavir-ir:inputs instruction))
       (when (and (typep input 'cleavir-ir:lexical-location)
                  (null (cleavir-ir:defining-instructions input)))
         (error "~s is used but never defined" input))))
   mir))

(defun compile-ast (client ast)
  (eliminate-fdefinition-asts ast)
  (let* ((cleavir-cst-to-ast::*origin* nil)
         (parameter-ast
           (make-instance 'cleavir-ast:lexical-ast
             :name (gensym))))
    (multiple-value-bind (hoisted-ast load-time-value-count)
        (hoist-load-time-value ast parameter-ast)
      (let* ((wrapped-ast (make-instance 'cleavir-ast:function-ast
                            :lambda-list (list parameter-ast)
                            :body-ast hoisted-ast))
             (hir (cleavir-ast-to-hir:compile-toplevel-unhoisted client wrapped-ast))
             (code-object
               (make-instance 'code-object
                 :ast ast
                 :ir hir)))
        (adjust-array (constants code-object) load-time-value-count
                      :fill-pointer t)
        (cleavir-partial-inlining:do-inlining hir)
        (sicl-argument-processing:process-parameters hir)
        (sicl-hir-transformations:eliminate-fixed-to-multiple-instructions hir)
        (sicl-hir-transformations:eliminate-multiple-to-fixed-instructions hir)
        (cleavir-hir-transformations::process-captured-variables hir)
        ;; Replacing aliases does not appear to have a great effect when
        ;; code generation is disabled.  Try removing this commented line
        ;; when code generation is again enabled.
        ;; (cleavir-hir-transformations:replace-aliases hir)
        (sicl-hir-transformations:eliminate-create-cell-instructions hir)
        (sicl-hir-transformations:eliminate-fetch-instructions hir)
        (sicl-hir-transformations:eliminate-read-cell-instructions hir)
        (sicl-hir-transformations:eliminate-write-cell-instructions hir)
        (process-constant-inputs code-object)
        (let ((constants (constants code-object)))
          (cleavir-ir:map-instructions-arbitrary-order
           (lambda (instruction)
             (when (typep instruction 'sicl-ir:load-constant-instruction)
               (setf (sicl-ir:constants instruction) constants)))
           hir))
        (cleavir-remove-useless-instructions:remove-useless-instructions hir)
        (establish-call-sites code-object)
        (setf (hir-thunks code-object)
              (sicl-hir-evaluator:top-level-hir-to-host-function client hir))
        (sicl-hir-transformations:eliminate-append-values-instructions hir)
        (check-every-location-is-defined hir)
        ; (sicl-hir-to-mir:hir-to-mir client code-object)
        ; (sicl-mir-to-lir:mir-to-lir client hir)
        ;; (multiple-value-bind (instructions label-map)
        ;;   (cluster:assemble (sicl-code-generation:generate-code hir2))
        code-object))))
