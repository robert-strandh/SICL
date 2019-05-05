(cl:in-package #:sicl-hir-to-cl)

(defun make-code-bindings (initial-instruction context)
  (let ((enter-instructions (sort-functions initial-instruction)))
    (loop for enter-instruction in (butlast enter-instructions)
          collect `(,(gethash enter-instruction (function-names context))
                    ,(translate-enter-instruction enter-instruction context)))))

(defun hir-to-cl (initial-instruction)
  (let ((enter-instructions (sort-functions initial-instruction))
        (context (make-instance 'context))
        (lexical-locations (find-lexical-locations initial-instruction))
        (successor (first (cleavir-ir:successors initial-instruction)))
        (*static-environment-variable* (gensym "static-environment"))
        (*top-level-function-parameter* (gensym "function-cell")))
    (loop for enter-instruction in (butlast enter-instructions)
          do (setf (gethash enter-instruction (function-names context))
                   (gensym "code")))
    `(lambda (,*top-level-function-parameter*)
       (let* ((,(static-env-function-var context)
                (car (funcall ,*top-level-function-parameter* 'static-environment-function)))
              ,@(make-code-bindings initial-instruction context)
              ,@(mapcar #'cleavir-ir:name lexical-locations)
              ,(values-location context)
              (,*static-environment-variable*
                (vector nil
                        (car (funcall ,*top-level-function-parameter*
                                      'enclose)))))
         (declare (ignore ,(cleavir-ir:name
                            (cleavir-ir:dynamic-environment-location initial-instruction))))
         (declare (ignorable ,(cleavir-ir:name
                               (first (cleavir-ir:outputs initial-instruction)))))
         (block ,(block-name context)
           (tagbody ,@(translate successor context)))))))

(defmethod translate
    ((instruction sicl-hir-transformations::find-function-cell-instruction) context)
  (let* ((name (sicl-hir-transformations::name instruction))
         (output (first (cleavir-ir:outputs instruction)))
         (output-name (cleavir-ir:name output))
         (successor (first (cleavir-ir:successors instruction))))
  `((setq ,output-name
          (funcall ,*top-level-function-parameter* ',name))
    ,@(translate successor context))))
