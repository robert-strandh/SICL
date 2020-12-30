(cl:in-package #:sicl-data-and-control-flow)

(defmacro define-modify-macro
    (name lambda-list function &optional documentation)
  (let* ((parsed-lambda-list
           (cleavir-code-utilities:parse-define-modify-macro-lambda-list lambda-list))
         (required (cleavir-code-utilities:required parsed-lambda-list))
         (optionals (cleavir-code-utilities:optionals parsed-lambda-list))
         (rest (cleavir-code-utilities:rest-body parsed-lambda-list))
         (place-var (gensym)))
    `(defmacro ,name (,place-var ,@lambda-list)
       ,@(if (null documentation) '() (list documentation))
       (let ((argument-forms
               (list* ,@required
                      ,@(if (eq optionals :none)
                            '()
                            (mapcar #'first optionals))
                      ,(if (eq rest :none)
                           '()
                           rest))))
         (multiple-value-bind
               (vars vals store-vars writer-form reader-form)
             (get-setf-expansion ,place-var)
           `(let ,(loop for var in vars
                        for val in vals
                        collect `(,var ,val))
              (let ((,(first store-vars)
                      (,',function ,reader-form ,@argument-forms)))
                ,writer-form)))))))
