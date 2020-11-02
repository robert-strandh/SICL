(cl:in-package #:sicl-ast-evaluator)

(defun cst-to-ast (client cst environment)
   (handler-bind
       ((trucler:no-function-description
          (lambda (condition)
            (warn "Unknown function ~s" (trucler:name condition))
            (invoke-restart 'cleavir-cst-to-ast:consider-global)))
        (trucler:no-variable-description
          (lambda (condition)
            (warn "Unknown variable ~s" (trucler:name condition))
            (invoke-restart 'cleavir-cst-to-ast:consider-special)))
        (cleavir-cst-to-ast::encapsulated-condition
          (lambda (condition)
            (declare (ignore condition))
            (invoke-restart 'cleavir-cst-to-ast:signal-original-condition))))
     (cleavir-cst-to-ast:cst-to-ast
      client cst environment)))

;;; This variable will hold a list of CONS cells, where the CAR of
;;; each cell is the name of a function, and the CDR is the name of a
;;; variable that will hold the function cell of the function with
;;; that name.
(defvar *function-cells*)

(defun translate-top-level-ast (ast)
  (let* ((table (make-hash-table :test #'eq))
         (lexical-environment (list table))
         (function-cell-finder-var (gensym))
         (*run-time-environment-name* (gensym))
         (*function-cells* '())
         (code (translate-ast ast lexical-environment)))
    `(lambda (,*run-time-environment-name*)
       (declare (ignorable ,*run-time-environment-name*))
       (declare (optimize (speed 0) (compilation-speed 3) (debug 0) (safety 3) (space 0)))
       #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
       (let* ((,function-cell-finder-var
                (sicl-environment:fdefinition
                 (sicl-environment:client ,*run-time-environment-name*)
                 ,*run-time-environment-name*
                 'sicl-data-and-control-flow:function-cell))
              ,@(loop for (function-name . variable-name) in *function-cells*
                      collect `(,variable-name
                                (funcall ,function-cell-finder-var ',function-name))))
         (declare (ignorable ,function-cell-finder-var))
         ,code))))

(defun translate-code (client environment cst)
  (let* ((ast1 (cst-to-ast client cst environment))
         (ast2 (cleavir-ast-transformations:hoist-load-time-value ast1)))
    (translate-top-level-ast ast2)))
