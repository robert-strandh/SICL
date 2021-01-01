(cl:in-package #:cleavir-cst-to-ast)

(defun var-to-lexical (var-cst)
  (let* ((raw (cst:raw var-cst))
         (name (make-symbol (string-downcase raw))))
    (cleavir-ast:make-ast 'cleavir-ast:lexical-ast
      :name name)))

(defun init-var-to-lexicals (var-cst supplied-p-cst)
  (list (var-to-lexical var-cst)
        (if (null supplied-p-cst)
            (cleavir-ast:make-ast 'cleavir-ast:lexical-ast :name (gensym))
            (var-to-lexical supplied-p-cst))))

(defgeneric entries-from-parameter-group (parameter-group))

(defmethod entries-from-parameter-group
    ((parameter-group cst:multi-parameter-group-mixin))
  (loop for parameter in (cst:parameters parameter-group)
        collect (entry-from-parameter parameter)))

(defmethod entries-from-parameter-group
    ((parameter-group cst:ordinary-rest-parameter-group))
  (list (entry-from-parameter (cst:parameter parameter-group))))

(defmethod entries-from-parameter-group
    ((parameter-group cst:aux-parameter-group))
  ;; Don't need any.
  nil)

(defgeneric entry-from-parameter (parameter))

(defmethod entry-from-parameter ((parameter cst:simple-variable))
  (var-to-lexical (cst:name parameter)))

(defmethod entry-from-parameter ((parameter cst:ordinary-optional-parameter))
  (init-var-to-lexicals (cst:name parameter)
                        (cst:supplied-p parameter)))

(defmethod entry-from-parameter ((parameter cst:ordinary-key-parameter))
  (init-var-to-lexicals (cst:name parameter)
                        (cst:supplied-p parameter)))

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
(defun lambda-list-from-parameter-groups (parameter-groups)
  (loop for group in parameter-groups
        for entries = (entries-from-parameter-group group)
        for lambda-list-part = (lambda-list-from-parameter-group group entries)
        appending lambda-list-part into lambda-list
        collecting entries into components
        finally (return (values lambda-list components))))
