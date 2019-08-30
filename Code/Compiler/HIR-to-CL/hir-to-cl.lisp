(cl:in-package #:sicl-hir-to-cl)

(defun make-code-bindings (client initial-instruction context)
  (let ((enter-instructions (sort-functions initial-instruction)))
    (loop for enter-instruction in (butlast enter-instructions)
          do (setf (gethash enter-instruction (function-names context))
                   (gensym "code")))
    (loop for enter-instruction in (butlast enter-instructions)
          collect `(,(gethash enter-instruction (function-names context))
                    ,(translate-enter-instruction client enter-instruction context)))))

(defun find-values-locations (initial-instruction)
  (let ((result (make-hash-table :test #'eq)))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (loop with inputs = (cleavir-ir:inputs instruction)
             with outputs = (cleavir-ir:outputs instruction)
             for datum in (append inputs outputs)
             when (typep datum 'cleavir-ir:values-location)
               do (let ((symbol (gethash datum result)))
                    (if (null symbol)
                        (setf (gethash datum result) (gensym))
                        symbol))))
     initial-instruction)
    result))

(defun all-values-location-names (table)
  (loop for name being each hash-value of table
        collect name))

(defun values-location-name (values-location context)
  (gethash values-location (values-locations context)))

(defun hir-to-cl (client initial-instruction)
  (let* ((static-environment-output
           (cleavir-ir:static-environment initial-instruction))
         (values-locations (find-values-locations initial-instruction))
         (context (make-instance 'context :values-locations values-locations))
         (values-location-names (all-values-location-names values-locations))
         (lexical-locations (find-valid-lexical-locations
                             initial-instruction static-environment-output))
         (lexical-location-names (mapcar #'cleavir-ir:name lexical-locations))
         (successor (first (cleavir-ir:successors initial-instruction)))
         (*static-environment-variable* (cleavir-ir:name static-environment-output))
         (*top-level-function-parameter* (gensym "function-cell"))
         (basic-blocks (compute-basic-blocks initial-instruction))
         (*basic-blocks-in-dynamic-environment* (make-hash-table :test #'eq))
         (*basic-block-of-leader* (make-hash-table :test #'eq))
         (*tag-of-basic-block* (make-hash-table :test #'eq)))
    (process-basic-blocks basic-blocks)
    `(lambda (,*top-level-function-parameter* ,*static-environment-variable*)
       #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
       (declare (ignorable ,*static-environment-variable*))
       (let* ((,(static-env-function-var context)
                (car (funcall ,*top-level-function-parameter* 'static-environment-function)))
              ,@values-location-names
              ,@(make-code-bindings client initial-instruction context)
              ,@lexical-location-names
              (source nil))
         (declare (ignorable ,(cleavir-ir:name
                               (cleavir-ir:dynamic-environment-location initial-instruction))))
         (declare (ignorable ,@lexical-location-names))
         (declare (ignorable ,(static-env-function-var context)))
         (declare (ignorable ,@values-location-names))
         (declare (ignorable source))
         (block ,(block-name context)
           (tagbody (go ,(tag-of-basic-block (basic-block-of-leader successor)))
              ,@(tagged-basic-blocks successor client context)))))))

(defmethod translate
    (client (instruction sicl-hir-transformations::find-function-cell-instruction) context)
  (let* ((name (sicl-hir-transformations::name instruction))
         (output (first (cleavir-ir:outputs instruction)))
         (output-name (cleavir-ir:name output)))
  `((setq ,output-name
          (funcall ,*top-level-function-parameter* ',name)))))
