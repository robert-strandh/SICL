(cl:in-package #:cleavir-cst-to-ast)

(defun var-to-lexical (var-cst dynamic-environment-ast)
  (let* ((raw (cst:raw var-cst))
         (name (make-symbol (string-downcase raw))))
    (make-instance 'cleavir-ast:lexical-ast
      :name name
      :dynamic-environment-input-ast dynamic-environment-ast)))

(defun init-var-to-lexicals (var-cst supplied-p-cst dynamic-environment-ast)
  (list (var-to-lexical var-cst dynamic-environment-ast)
        (if (null supplied-p-cst)
            (make-instance 'cleavir-ast:lexical-ast :name (gensym))
            (var-to-lexical supplied-p-cst dynamic-environment-ast))))

(defgeneric entries-from-parameter-group (parameter-group dynamic-environment-ast))

(defmethod entries-from-parameter-group
    ((parameter-group cst:multi-parameter-group-mixin)
     dynamic-environment-ast)
  (loop for parameter in (cst:parameters parameter-group)
        collect (entry-from-parameter parameter dynamic-environment-ast)))

(defmethod entries-from-parameter-group
    ((parameter-group cst:ordinary-rest-parameter-group)
     dynamic-environment-ast)
  (list (entry-from-parameter (cst:parameter parameter-group) dynamic-environment-ast)))

(defmethod entries-from-parameter-group
    ((parameter-group cst:aux-parameter-group)
     dynamic-environment-ast)
  ;; Don't need any.
  nil)

(defgeneric entry-from-parameter (parameter dynamic-environment-ast))

(defmethod entry-from-parameter ((parameter cst:simple-variable)
                                 dynamic-environment-ast)
  (var-to-lexical (cst:name parameter) dynamic-environment-ast))

(defmethod entry-from-parameter ((parameter cst:ordinary-optional-parameter)
                                 dynamic-environment-ast)
  (init-var-to-lexicals (cst:name parameter)
                        (cst:supplied-p parameter)
                        dynamic-environment-ast))

(defmethod entry-from-parameter ((parameter cst:ordinary-key-parameter)
                                 dynamic-environment-ast)
  (init-var-to-lexicals (cst:name parameter)
                        (cst:supplied-p parameter)
                        dynamic-environment-ast))

(defgeneric lambda-list-from-parameter-group (parameter-group entries))

(defmethod lambda-list-from-parameter-group
    ((parameter-group cst:ordinary-required-parameter-group)
     entries)
  (values entries entries))

(defmethod lambda-list-from-parameter-group
    ((parameter-group cst:optional-parameter-group)
     entries)
  (values (cons '&optional entries) entries))

(defmethod lambda-list-from-parameter-group
    ((parameter-group cst:ordinary-rest-parameter-group)
     entries)
  (values (cons '&rest entries) entries))

(defmethod lambda-list-from-parameter-group
    ((parameter-group cst:key-parameter-group)
     entries)
  (values (append '(&key)
                  (mapcar (lambda (entry parameter)
                            (cons (cst:raw (cst:keyword parameter)) entry))
                          entries (cst:parameters parameter-group))
                  (if (cst:allow-other-keys parameter-group)
                      '(&allow-other-keys)
                      '()))
          entries))

(defmethod lambda-list-from-parameter-group
    ((parameter-group cst:aux-parameter-group)
     entries)
  ;; &aux doesn't contribute to the function-ast's lambda-list.
  (values '() entries))

;;; Given a list of parameter groups, return a lambda list suitable
;;; for the FUNCTION-AST, as well as a list of lists of lexical variables.
(defun lambda-list-from-parameter-groups (parameter-groups dynamic-environment-ast)
  (loop for group in parameter-groups
        for entries = (entries-from-parameter-group group dynamic-environment-ast)
        for lambda-list-part = (lambda-list-from-parameter-group group entries)
        appending lambda-list-part into lambda-list
        collecting entries into components
        finally (return (values lambda-list components))))
