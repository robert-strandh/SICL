(cl:in-package #:sicl-package)

(defmacro do-symbols
    ((symbol-variable
      &optional
        (package-form '*package*)
        (result-form 'NIL))
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
                   do (,funcation-name symbol))
             (loop for symbol being each hash-value of (internal-symbols ,package-var)
                   do (,funcation-name symbol))
             (loop for used-package in (use-list ,package-var)
                   do (loop for symbol being each hash-value of (external-symbols used-package)
                            unless (member symbol
                                           (shadowing-symbols ,package-var)
                                           :test #'eq)
                              do (,funcation-name symbol)))))
         ,result-form))))
