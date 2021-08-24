(cl:in-package #:sicl-boot-phase-1)

(defun import-function (client environment function-name)
  (setf (env:fdefinition client environment function-name)
        (fdefinition function-name)))

(defun import-conditionals-support (client environment)
  (import-function client environment 'sicl-conditionals:or-expander)
  (import-function client environment 'sicl-conditionals:and-expander)
  (import-function client environment 'sicl-conditionals:cond-expander)
  (import-function client environment 'sicl-conditionals:case-expander)
  (import-function client environment 'sicl-conditionals:ecase-expander)
  (import-function client environment 'sicl-conditionals:ccase-expander)
  (import-function client environment 'sicl-conditionals:typecase-expander)
  (import-function client environment 'sicl-conditionals:etypecase-expander)
  (import-function client environment 'sicl-conditionals:ctypecase-expander))

(defun import-cleavir-primops (client environment)
  (setf (sicl-environment:special-operator
         client environment 'cleavir-primop:multiple-value-call)
        '(:special-operator t)))

(defun import-code-utilities (client environment)
  (import-function client environment 'cleavir-code-utilities:parse-macro)
  (import-function client environment 'cleavir-code-utilities:separate-function-body)
  (import-function client environment 'cleavir-code-utilities:list-structure))

(defun import-trucler-functions (client environment)
  (import-function client environment 'trucler:symbol-macro-expansion)
  (import-function client environment 'trucler:macro-function))

(defun define-defgeneric-expander (client environment)
  (setf (env:fdefinition client environment 'sicl-clos:defgeneric-expander)
        (lambda (name lambda-list options-and-methods environment)
          (declare (ignore environment))
          (assert (or (null options-and-methods)
                      (and (null (cdr options-and-methods))
                           (eq (caar options-and-methods)
                               :argument-precedence-order))))
          `(ensure-generic-function
            ',name
            :lambda-list ',lambda-list
            ,@(if (null options-and-methods)
                  '()
                  `(:argument-precedence-order ',(cdar options-and-methods)))
            :environment (env:global-environment)))))
