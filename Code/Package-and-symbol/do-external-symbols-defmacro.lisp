(cl:in-package #:sicl-package)

(defmacro do-external-symbols
    ((symbol-variable
      &optional
        (package-form '*package*)
        (result-form 'nil))
     &body body)
  (let ((function-name (gensym))
        (package-var (gensym)))
    (multiple-value-bind (declarations body-forms)
        (cleavir-code-utilities:separate-ordinary-body body)
      `(block nil
         (flet ((,function-name (,symbol-variable)
                  (locally ,@declarations
                    (tagbody
                       ,@body-forms))))
           (let ((,package-var (package-designator-to-package ,package-form)))
             (loop for symbol being each hash-value of (external-symbols ,package-var)
                   do (,function-name symbol))))
         ,result-form))))
