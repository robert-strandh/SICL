(cl:in-package #:sicl-data-and-control-flow)

(defmacro define-modify-macro
    (name lambda-list function &optional documentation)
  (let* ((canonicalized-lambda-list
           (ecclesia:canonicalize-define-modify-macro-lambda-list
            lambda-list))
         (required
           (ecclesia:extract-required
            canonicalized-lambda-list))
         (optionals
           (ecclesia:extract-named-group
            canonicalized-lambda-list '&optional))
         (rest
           (ecclesia:extract-named-group
            canonicalized-lambda-list '&rest))
         (place-var (gensym)))
    `(defmacro ,name (,place-var ,@lambda-list)
       ,@(if (null documentation) '() (list documentation))
       (let ((argument-forms
               (list* ,@required
                      ,@(if (null optionals)
                            '()
                            (mapcar #'first (rest optionals)))
                      ,(if (null rest)
                           '()
                           (second rest)))))
         (multiple-value-bind
               (vars vals store-vars writer-form reader-form)
             (get-setf-expansion ,place-var)
           `(let ,(loop for var in vars
                        for val in vals
                        collect `(,var ,val))
              (let ((,(first store-vars)
                      (,',function ,reader-form ,@argument-forms)))
                ,writer-form)))))))
