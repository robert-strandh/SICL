(cl:in-package #:cleavir-cst-to-ast-test)

(defclass environment () ())

(defmethod cleavir-environment:optimize-info ((environment environment))
  (make-instance 'cleavir-environment:optimize-info
    :optimize '((speed 0) (compilation-speed 0) (space 0) (debug 3) (safety 3))
    :policy '()))

(defmethod cleavir-environment:function-info
    ((environment environment) function-name)
  (cond ((or (and (symbolp function-name)
                  (eq (symbol-package function-name)
                      (find-package 'cleavir-primop))
                  (not (eq function-name
                           'cleavir-primop:call-with-variable-bound)))
             (and (symbolp function-name)
                  (special-operator-p function-name)))
         (make-instance 'cleavir-environment:special-operator-info
           :name function-name))
        ((and (symbolp function-name)
              (eq (symbol-package function-name)
                  (find-package 'common-lisp))
              (not (null (macro-function function-name))))
         (make-instance 'cleavir-environment:global-macro-info
           :name function-name
           :expander (macro-function function-name)
           :compiler-macro nil))
        ((and (symbolp function-name)
              (eq (symbol-package function-name)
                  (find-package 'common-lisp))
              (typep (ignore-errors (fdefinition function-name)) 'function))
         (make-instance 'cleavir-environment:global-function-info
           :name function-name
           :dynamic-extent nil
           :ast nil
           :ignore nil
           :compiler-macro nil
           :inline nil
           :type t))
        (t nil)))

(defmethod cleavir-environment:variable-info ((environment environment) symbol)
  (if (member symbol '(*special1* *special2*))
      (make-instance 'cleavir-environment:special-variable-info
        :global-p t
        :ignore nil
        :name symbol)
      nil))

(defmethod cleavir-environment:eval (form (env1 environment) (env2 environment))
  (eval form))
