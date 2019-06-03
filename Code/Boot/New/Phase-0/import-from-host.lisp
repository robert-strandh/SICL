(cl:in-package #:sicl-boot-phase-0)

(defun import-standard-common-lisp-functions (environment)
  (do-symbols (symbol (find-package '#:common-lisp))
    (when (fboundp symbol)
      (unless (special-operator-p symbol)
        (let ((definition (fdefinition symbol)))
          (when (functionp definition)
            (setf (sicl-genv:fdefinition symbol environment) definition)))))
    (when (fboundp `(setf ,symbol))
      (setf (sicl-genv:fdefinition `(setf ,symbol) environment)
            (fdefinition `(setf ,symbol))))))

(defun define-standard-common-lisp-variables (environment)
  (setf (sicl-genv:special-variable '*macroexpand-hook* environment t)
        *macroexpand-hook*))

(defun define-standard-common-lisp-special-operators (environment)
  (do-symbols (symbol (find-package '#:common-lisp))
    (when (special-operator-p symbol)
      (setf (sicl-genv:special-operator symbol environment) t))))

(defun define-cleavir-primops (environment)
  (do-symbols (symbol (find-package '#:cleavir-primop))
    (setf (sicl-genv:special-operator symbol environment) t)))

(defun import-from-cleavir-code-utilities (environment)
  (loop for name in '(cleavir-code-utilities:parse-macro
                      cleavir-code-utilities:separate-ordinary-body
                      cleavir-code-utilities:list-structure
                      cleavir-code-utilities:proper-list-p)
        do (setf (sicl-genv:fdefinition name environment)
                 (fdefinition name))))

(defun import-macro-expanders (environment)
  (loop for name in '(sicl-data-and-control-flow:defun-expander
                      sicl-conditionals:or-expander
                      sicl-conditionals:and-expander
                      sicl-conditionals:cond-expander
                      sicl-conditionals:case-expander
                      sicl-conditionals:ecase-expander
                      sicl-conditionals:ccase-expander
                      sicl-conditionals:typecase-expander
                      sicl-conditionals:etypecase-expander
                      sicl-conditionals:ctypecase-expander
                      sicl-standard-environment-macros:defconstant-expander
                      sicl-standard-environment-macros:defvar-expander
                      sicl-standard-environment-macros:defparameter-expander
                      sicl-standard-environment-macros:deftype-expander
                      sicl-standard-environment-macros:define-compiler-macro-expander
                      sicl-evaluation-and-compilation:declaim-expander
                      sicl-loop:expand-body
                      sicl-cons:push-expander
                      sicl-cons:pop-expander
                      sicl-cons:pushnew-expander
                      sicl-cons:remf-expander
                      sicl-data-and-control-flow:psetf-expander
                      sicl-data-and-control-flow:rotatef-expander
                      sicl-data-and-control-flow:destructuring-bind-expander
                      sicl-data-and-control-flow:shiftf-expander
                      sicl-iteration:dotimes-expander
                      sicl-iteration:dolist-expander
                      sicl-iteration:do-dostar-expander
                      sicl-clos:defclass-expander
                      sicl-clos:defgeneric-expander)
        do (setf (sicl-genv:fdefinition name environment)
                 (fdefinition name))))

(defun import-sicl-envrionment-functions (environment)
  (loop for name in '((setf sicl-genv:fdefinition)
                      (setf sicl-genv:function-type)
                      (setf sicl-genv:function-lambda-list)
                      sicl-genv:get-setf-expansion)
        do (setf (sicl-genv:fdefinition name environment)
                 (fdefinition name))))

(defun import-from-trucler (environment)
  (loop for name in '(trucler:global-environment
                      trucler:symbol-macro-expansion
                      trucler:macro-function
                      trucler-reference:global-environment)
        do (setf (sicl-genv:fdefinition name environment)
                 (fdefinition name))))

(defun import-from-closer-mop (environment)
  (loop for name in '(#:ensure-class)
        for closer-mop-symbol = (intern (string name) '#:closer-mop)
        for sicl-clos-symbol = (intern (string name) '#:sicl-clos)
        do (setf (sicl-genv:fdefinition sicl-clos-symbol environment)
                 (fdefinition closer-mop-symbol))))

(defun import-from-sicl-clos (environment)
  (setf (sicl-genv:special-variable 'sicl-clos::*class-unique-number* environment t)
        'sicl-clos::*class-unique-number*))

(defun import-from-host (environment)
  (host-load "Data-and-control-flow/defun-support.lisp")
  (host-load "Environment/macro-support.lisp")
  (host-load "CLOS/defclass-support.lisp")
  (host-load "CLOS/defgeneric-support.lisp")
  (host-load "CLOS/make-method-lambda-support.lisp")
  (host-load "Boot/Phase-0/create-method-lambda.lisp")
  (host-load "CLOS/defmethod-support.lisp")
  (import-standard-common-lisp-functions environment)
  (define-standard-common-lisp-variables environment)
  (define-standard-common-lisp-special-operators environment)
  (define-cleavir-primops environment)
  (import-from-cleavir-code-utilities environment)
  (import-macro-expanders environment)
  (import-sicl-envrionment-functions environment)
  (import-from-trucler environment)
  (import-from-closer-mop environment)
  (import-from-sicl-clos environment))
