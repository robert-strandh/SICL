(cl:in-package #:sicl-boot)

(defgeneric loaded-files (environment))

(defgeneric (setf loaded-files) (loaded-files environment))

(defclass environment (env:run-time-environment)
  ((%name :initarg :name :reader name)
   ;; This slot holds an association list.  The key of an element is
   ;; the pathname naming a file that has been loaded into this
   ;; environment.  The value of an element is a universal time when
   ;; the file was loaded.
   (%loaded-files :initform '() :accessor loaded-files)
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

(defun define-macroexpand (environment)
  (setf (env:fdefinition (env:client environment) environment 'macroexpand-1)
        (lambda (form &optional (env environment))
          (etypecase form
            ((cons symbol)
             (let ((expander (trucler:macro-function (car form) env)))
               (if (null expander)
                   (values form nil)
                   (values (funcall expander form env) t))))
            (symbol
             (let ((expansion (trucler:symbol-macro-expansion form env)))
               (if (eq expansion form)
                   (values form nil)
                   (values expansion t)))))))
    (setf (env:fdefinition (env:client environment) environment 'macroexpand)
        (lambda (form &optional (env environment))
          (let ((result form)
                (e-p nil))
            (loop do (multiple-value-bind (expansion expanded-p)
                         (funcall (env:fdefinition (env:client environment) environment 'macroexpand-1)
                                  result env)
                       (if expanded-p
                           (setf result expansion
                                 e-p t)
                           (return (values result e-p)))))))))

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
  (setf (env:fdefinition
         client environment '(setf sicl-data-and-control-flow:setf-expander))
        (lambda (setf-expander name)
          (setf (env:setf-expander client environment name)
                setf-expander)))
  (setf (env:fdefinition
         client environment 'sicl-data-and-control-flow:setf-expander)
        (lambda (name)
          (env:setf-expander client environment name)))
  (setf (env:fdefinition client environment '(setf fdefinition))
        (lambda (function name)
          (setf (env:fdefinition client environment name)
                function)))
  (setf (env:fdefinition client environment 'fdefinition)
        (lambda (name)
          (env:fdefinition client environment name)))
  (setf (env:fdefinition client environment '(setf symbol-function))
        (lambda (function name)
          (setf (env:fdefinition client environment name)
                function)))
  (setf (env:fdefinition client environment 'symbol-function)
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
         '(setf find-class))
        (lambda (new-class symbol &optional errorp env)
          (declare (ignore errorp env))
          (setf (env:find-class client environment symbol)
                new-class)))
  (setf (env:fdefinition
         client
         environment
         'find-package)
        (lambda (name)
          (env:find-package client environment (string name))))
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
  (setf (env:fdefinition client environment 'sicl-symbol:variable-cell)
        (lambda (name)
          (env:variable-cell client environment name)))
  (import-functions-from-host
   '(env:fdefinition
     env:compiler-macro-function (setf env:compiler-macro-function))
   environment)
  (setf (env:fdefinition
         client
         environment
         'sicl-type:type-expander)
        (lambda (name)
          (env:type-expander client environment name)))
  (define-macroexpand environment))

(defun import-standard-functions (environment)
  (import-functions-from-host
   '(;; Numbers
     + - * / < <= = > >= /= floor 1+ 1-
     plusp minusp integerp numberp
     logand logior logxor ash integer-length
     zerop oddp evenp max min expt abs ldb byte random
     mod truncate
     log sqrt
     ;; CONSes
     cons list list* append nconc make-list copy-list subst
     consp atom listp null endp
     car cdr caar cadr cdar cddr
     caaar caadr cadar caddr cdaar cdadr cddar cdddr nthcdr
     first second third fourth fifth nth rest
     rplaca rplacd
     member
     mapcar mapc
     getf
     assoc rassoc acons
     set-difference set-exclusive-or union adjoin
     last butlast list-length
     ;; Data and control flow
     apply not eq eql equal values constantly identity some
     every
     ;; Evaluation and compilation
     constantp
     ;; Symbols
     gensym symbolp
     ;; Characters
     characterp char-code code-char char= char-name standard-char-p
     ;; Strings
     stringp char schar string string=
     ;; Types and classes
     coerce
     ;; Sequence
     elt length count reverse position subseq sort remove find
     reduce remove-duplicates find-if find-if-not position-if
     position-if-not nreverse replace delete copy-seq mismatch
     ;; For debugging
     format
     ;; Conditions
     error break)
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
    (setf (env:fdefinition client environment '(setf second))
          (lambda (new-value cons)
            (setf (second cons) new-value)))
    (setf (env:fdefinition client environment '(setf cddr))
          (lambda (new-value cons)
            (setf (cddr cons) new-value)))
    (setf (env:fdefinition client environment '(setf nth))
          (lambda (new-value n list)
            (setf (nth n list) new-value)))
    (setf (env:fdefinition client environment '(setf elt))
          (lambda (new-value sequence index)
            (setf (elt sequence index) new-value)))))

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

(defun import-run-time-functions (environment)
  (import-functions-from-host
   '(sicl-run-time:symbol-value
     (setf sicl-run-time:symbol-value)
     sicl-run-time:boundp
     sicl-run-time:makunbound)
   environment))

(defun define-constants (client environment)
  (setf (env:constant-variable client environment 'most-positive-fixnum)
        (1- (expt 2 63)))
  (setf (env:constant-variable client environment 'most-negative-fixnum)
        (- (expt 2 63))))

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
    (import-run-time-functions environment)
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
    (setf (env:fdefinition client environment 'host-symbol-name)
          #'symbol-name)
    (setf (env:fdefinition client environment 'host-symbol-package)
          #'symbol-package)
    (setf (env:fdefinition client environment 'host-array-dimensions)
          #'array-dimensions)
    (setf (env:fdefinition client environment 'host-package-name)
          #'package-name)
    (define-special-operators environment)
    (define-primops environment)
    (define-setf-functions environment)
    (define-environment-functions client environment)
    (define-constants client environment)
    (setf (env:special-variable client environment '*trace-output* t)
          *trace-output*)
    ;; Make DEFPACKAGE create a package in the host environment
    (setf (env:macro-function client environment 'defpackage)
          (lambda (form env)
            (declare (ignore env))
            (eval form)
            nil))
    ;; Make it possible to inspect from the REPL
    (import-functions-from-host '(clouseau:inspect) environment)
    ;; Fake PROGV for now.
    (setf (env:macro-function (env:client environment) environment 'progv)
          (lambda (form env)
            (declare (ignore env))
            (cons 'progn (rest form))))))

(defmethod print-object ((object environment) stream)
  (print-unreadable-object (object stream)
    (format stream "Environment ~a" (name object))))
