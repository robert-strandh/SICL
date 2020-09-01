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

(defun translate-code (client environment cst)
  (let* ((ast (cst-to-ast client cst environment))
         (table (make-hash-table :test #'eq))
         (lexical-environment (list table)))
    (let ((*run-time-environment-name* (gensym)))
      `(lambda (,*run-time-environment-name*)
         (declare (ignorable ,*run-time-environment-name*))
         #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
         ,(translate-ast ast environment lexical-environment)))))
