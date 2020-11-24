(cl:in-package #:sicl-boot)

(defclass environment (env:run-time-environment)
  ((%name :initarg :name :reader name)
   ;; This slot holds a list of override entries.  Such an entry is a
   ;; CONS cell, where the CAR of the CONS cell is a function name,
   ;; and the CDR of the CONS cell is a FUNCTION CELL (which is
   ;; another CONS cell)
   ;;
   ;; The list of override entries affects two things.  The main
   ;; purpose is that when a FASL is loaded into the environment, the
   ;; function FUNCTION CELL first looks in this list before
   ;; consulting the normal environment entries.  
   (%overridden-function-cells
    :initform '()
    :accessor overridden-function-cells))
  (:default-initargs :client (make-instance 'client)))

(defmethod trucler:restrict-for-macrolet-expander
    (client (environment environment))
  environment)

(defun define-environment-functions (client environment)
  (setf (env:fdefinition client environment 'fboundp)
        (lambda (name)
          (env:fboundp client environment name)))
  (setf (env:fdefinition client environment 'special-operator-p)
        (lambda (name)
          (env:special-operator client environment name)))
  (setf (env:fdefinition client environment '(setf macro-function))
        (lambda (macro-function name &optional env)
          (assert (null env))
          (setf (env:macro-function client environment name)
                macro-function)))
  (setf (env:fdefinition client environment 'macro-function)
        (lambda (name &optional env)
          (assert (null env))
          (env:macro-function client environment name)))
  (setf (env:fdefinition client environment '(setf fdefinition))
        (lambda (function name)
          (setf (env:fdefinition client environment name)
                function)))
  (setf (env:fdefinition client environment 'fdefinition)
        (lambda (name)
          (env:fdefinition client environment name)))
  (setf (env:fdefinition client environment 'get-setf-expansion)
        (lambda (place &optional env)
          (declare (ignore env))
          (env:get-setf-expansion client environment place)))
  (setf (env:fdefinition
         client environment '(setf sicl-data-and-control-flow:special-variable))
        (lambda (value name set-if-assigned-p)
          (setf (env:special-variable client environment name set-if-assigned-p)
                value)))
  (setf (env:fdefinition
         client environment 'symbol-value)
        (lambda (name)
          (sicl-ast-evaluator::symbol-value name environment)))
  (setf (env:fdefinition
         client environment '(setf sicl-data-and-control-flow:constant-variable))
        (lambda (value name)
          (setf (env:constant-variable client environment name)
                value)))
  (setf (env:fdefinition
         client
         environment
         'sicl-method-combination:find-method-combination-template)
        (lambda (name)
          (env:find-method-combination-template name environment)))
  (setf (env:fdefinition
         client
         environment
         '(setf sicl-method-combination:find-method-combination-template))
        (lambda (template name)
          (setf (env:find-method-combination-template name environment)
                template)))
  (setf (env:fdefinition
         client
         environment
         'find-class)
        (lambda (symbol &optional (errorp t) env)
          (declare (ignore env))
          (let ((class (env:find-class client environment symbol)))
            (if (and errorp (null class))
                (error "no class named ~s in ~s" symbol environment)
                class))))
  (setf (env:fdefinition
         client
         environment
         'sicl-data-and-control-flow:function-cell)
        (lambda (name)
          (let* ((override-entry
                   (find name (overridden-function-cells environment)
                         :key #'car :test #'equal))
                 (result (if (null override-entry)
                             (env:function-cell client environment name)
                             (cdr override-entry))))
            result)))
  (setf (env:fdefinition
         client
         environment
         'sicl-type:type-expander)
        (lambda (name)
          (env:type-expander client environment name))))

(defun import-standard-functions (environment)
  (import-functions-from-host
   '(;; Numbers
     + - * / < <= = > >= /= floor 1+ 1-
     plusp minusp integerp numberp
     logand
     zerop evenp
     ;; CONSes
     cons list list* append make-list copy-list
     consp atom listp null endp
     car cdr caar cadr cdar cddr
     caaar caadr cadar caddr cdaar cdadr cddar cdddr
     first second third fourth fifth nth rest
     rplaca rplacd
     member
     mapcar mapc
     getf
     assoc set-difference set-exclusive-or union adjoin
     last butlast
     ;; Data and control flow
     apply not eq eql equal values
     every
     ;; Symbols
     gensym symbolp
     ;; Characters
     characterp
     ;; Strings
     stringp
     ;; Types and classes
     coerce
     ;; Sequence
     elt length count reverse position subseq sort remove find
     reduce remove-duplicates find-if find-if-not position-if
     ;; For debugging
     format
     ;; Conditions
     error)
   environment))

(defun define-setf-functions (environment)
  (let ((client (env:client environment)))
    (setf (env:fdefinition client environment '(setf car))
          (lambda (new-value cons)
            (setf (car cons) new-value)))
    (setf (env:fdefinition client environment '(setf cdr))
          (lambda (new-value cons)
            (setf (cdr cons) new-value)))
    (setf (env:fdefinition client environment '(setf cadr))
          (lambda (new-value cons)
            (setf (cadr cons) new-value)))
    (setf (env:fdefinition client environment '(setf cddr))
          (lambda (new-value cons)
            (setf (cddr cons) new-value)))
    (setf (env:fdefinition client environment '(setf gethash))
          (lambda (new-value key table)
            (setf (gethash key table) new-value)))))

(defun define-special-operators (environment)
  (let ((client (env:client environment)))
    (do-external-symbols (symbol '#:common-lisp)
      (when (special-operator-p symbol)
        (setf (env:special-operator client environment symbol)
              '(special t))))))

(defun define-primops (environment)
  (let ((client (env:client environment)))
    (do-external-symbols (symbol '#:cleavir-primop)
      (setf (env:special-operator client environment symbol)
            '(special t)))))

(defmethod initialize-instance :after ((environment environment) &key)
  (let ((client (env:client environment)))
    (setf (env:fdefinition client environment 'env:global-environment)
          (lambda (&optional lexical-environment)
            (if (null lexical-environment)
                environment
                (trucler:global-environment client lexical-environment))))
    (setf (env:fdefinition client environment 'env:client)
          #'env:client)
    (import-standard-functions environment)
    (setf (env:fdefinition client environment 'funcall)
          (lambda (function-designator &rest arguments)
            (let ((function (if (symbolp function-designator)
                                (progn (print function-designator) (env:fdefinition client environment function-designator))
                                function-designator)))
              (apply function arguments))))
    (setf (env:fdefinition client environment 'apply)
          (lambda (function-designator &rest arguments)
            (let ((function (if (symbolp function-designator)
                                (env:fdefinition client environment function-designator)
                                function-designator)))
              (apply #'apply function arguments))))
    (define-special-operators environment)
    (define-primops environment)
    (define-setf-functions environment)
    (define-environment-functions client environment)
    (import-functions-from-host
     '(cleavir-code-utilities:proper-list-p)
     environment)
    (setf (env:special-variable client environment '*trace-output* t)
          *trace-output*)
    ;; Make it possible to inspect from the REPL
    (import-functions-from-host '(clouseau:inspect) environment)))

(defmethod print-object ((object environment) stream)
  (print-unreadable-object (object stream)
    (format stream "Environment ~a" (name object))))
