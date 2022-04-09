(cl:in-package #:sicl-boot-phase-1)

(defun import-conditionals-support (environment)
  (import-functions-from-host
   '(sicl-conditionals:or-expander
     sicl-conditionals:and-expander
     sicl-conditionals:cond-expander
     sicl-conditionals:case-expander
     sicl-conditionals:ecase-expander
     sicl-conditionals:ccase-expander
     sicl-conditionals:typecase-expander
     sicl-conditionals:etypecase-expander
     sicl-conditionals:ctypecase-expander)
   environment))

(defun import-cleavir-primops (client environment)
  (setf (sicl-environment:special-operator
         client environment 'cleavir-primop:multiple-value-call)
        '(:special-operator t)))

(defun import-code-utilities (environment)
  (import-functions-from-host
   '(cleavir-code-utilities:parse-macro
     cleavir-code-utilities:separate-function-body
     cleavir-code-utilities:list-structure)
   environment))

(defun import-trucler-functions (environment)
  (import-functions-from-host
   '(trucler:symbol-macro-expansion
     trucler:macro-function)
   environment))

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
