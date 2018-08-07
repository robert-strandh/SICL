(cl:in-package #:sicl-minimal-extrinsic-environment)

(defparameter *imported-functions*
  '(;; Functions on CONS cells.
    car cdr append list consp null not mapcar
    first second third rest
    ;; Functions for arithmetic
    + - * / floor ceiling round
    ;; Various functions
    coerce eq values
    ;; Funcions related to symbols
    gensym symbolp
    ;; Functions for calling functions
    funcall apply
    sicl-genv:find-package
    sicl-genv:get-setf-expansion
    (setf sicl-genv:function-type)
    (setf sicl-genv:fdefinition)
    cleavir-environment:global-environment
    cleavir-environment:symbol-macro-expansion
    cleavir-environment:macro-function
    cleavir-code-utilities:parse-macro
    cleavir-code-utilities:parse-deftype
    cleavir-code-utilities:proper-list-p
    cleavir-code-utilities:separate-ordinary-body
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
    sicl-data-and-control-flow:shiftf-expander
    sicl-data-and-control-flow:defun-expander
    sicl-loop:expand-body))

(defparameter *imported-variables*
  '(*package* *macroexpand-hook*))

(defparameter *imported-packages*
  '(#:common-lisp
    #:sicl-evaluation-and-compilation
    #:sicl-global-environment
    #:sicl-data-and-control-flow
    #:sicl-conditionals
    #:sicl-arithmetic
    #:sicl-loop
    #:sicl-cons
    #:sicl-standard-environment-macros
    #:sicl-standard-environment-functions))

(defun import-from-host (environment)
  (host-load "../../../Data-and-control-flow/defun-support.lisp")
  (host-load "../../../Data-and-control-flow/shiftf-support.lisp")
  (host-load "../../../Conditionals/support.lisp")
  (host-load "../../../Environment/macro-support.lisp")
  (loop for name in *imported-functions*
        do (setf (sicl-genv:fdefinition name environment) 
                 (fdefinition name)))
  ;; We can't import (SETF CAR) and (SETF CDR) directly, because they
  ;; might not be defined as functions.  They could be SETF expanders.
  (setf (sicl-genv:fdefinition '(setf car) environment)
        (lambda (new-value cons)
          (setf (car cons) new-value)))
  (setf (sicl-genv:fdefinition '(setf cdr) environment)
        (lambda (new-value cons)
          (setf (cdr cons) new-value)))
  (loop for symbol in *imported-variables*
        for boundp = (boundp symbol)
        do (setf (sicl-genv:special-variable symbol environment boundp)
                 (if boundp (cl:symbol-value symbol) nil)))
  ;; Import all standard special operators
  (do-symbols (symbol (find-package '#:common-lisp) nil)
    (when (special-operator-p symbol)
      (setf (sicl-genv:special-operator symbol environment) t)))
  ;; Import some packages
  (loop for name in *imported-packages*
        do (push (find-package name)
                 (sicl-genv:packages environment))))
