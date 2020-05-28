(cl:in-package #:sicl-boot-phase-7)

(defun enable-special-operators (environment)
  (do-symbols (symbol (find-package '#:cleavir-primop))
    (setf (sicl-genv:special-operator symbol environment) t)))

;;; We import most of the expander functions from the host at first.
;;; The reason is that expander function might need some of the
;;; macros, and the dependency might be circular.  So the strategy is
;;; to first import the expanders, then make all the macros work, and
;;; when everything works, load the expanders normally.
;;;
;;; It would seem that we could load the FASLs of the expander
;;; functions, compiled in environment E0, but some macros have
;;; temporary, incorrect definitions in E0, and in particular the
;;; DEFGENERIC macro.  So if a file defining an expander function
;;; contains a call to any of those macros with temporary definitions,
;;; then it will be incorrectly compiled.
;;;
;;; Occasionally, we can't use the host version of the expander
;;; function, because it calls some SICL-specific function, like for
;;; example GET-SETF-EXPANSION.  In that case, we must obviously load
;;; the support code rather than import it from the host.
;;;
;;; Large parts of this code are similar to that of the file
;;; fill-environment.lisp in phase 0, so it would seem that the
;;; environment E5 already contains most of the functionality.
;;; However, SICL simple functions are defined slightly differently in
;;; phase 0, so we need to do it over here.  Also, code generation
;;; will not be turned on in phase 0, so that's another reason to load
;;; some of the code again here.
(defun enable-macros (environment)
  ;; Enable DEFMACRO.
  (load-source "Evaluation-and-compilation/defmacro-defmacro.lisp" environment)
  ;; Enable LAMBDA.
  ;; There is no independent expander for LAMBDA, because it is so
  ;; simple.
  (load-source "Evaluation-and-compilation/lambda.lisp" environment)
  ;; Enable MULTIPLE-VALUE-BIND.
  ;; There is no independent expander for MULTIPLE-VALUE-BIND.,
  ;; because it is so simple.
  (load-source "Environment/multiple-value-bind.lisp" environment)
  ;; Enable MULTIPLE-VALUE-LIST.
  ;; There is no independent expander for MULTIPLE-VALUE-LIST.,
  ;; because it is so simple.
  (load-source "Data-and-control-flow/multiple-value-list-defmacro.lisp" environment)
  ;; Enable MULTIPLE-VALUE-CALL.
  ;; There is no independent expander for MULTIPLE-VALUE-CALL.,
  ;; because it is so simple.
  (load-source "Data-and-control-flow/multiple-value-call-defmacro.lisp" environment)
  ;; Enable LOOP.
  (sicl-boot:import-functions-from-host
   '(sicl-loop:expand-body sicl-loop::list-car sicl-loop::list-cdr)
     environment)
  ;; There is no independent expander for LOOP, because it is so
  ;; simple.
  (load-source "Loop/loop-defmacro.lisp" environment)
  ;; Enable RETURN.
  ;; There is no independent expander for RETURN, because it is so
  ;; simple.
  (load-source "Data-and-control-flow/return-defmacro.lisp" environment)
  ;; Enable PROG1 and PROG2.
  ;; There are no independent expanders for PROG1 and PROG2, because
  ;; they are so simple.
  (load-source "Data-and-control-flow/prog1-prog2-defmacro.lisp" environment)
  ;; Enable PROG and PROG*.
  ;; There are no independent expanders for PROG and PROG*, because
  ;; they are so simple.
  (load-source "Data-and-control-flow/prog-progstar-defmacro.lisp" environment)
  ;; enable INCF and DECF.
  ;; There are no independent expanders for INCF and DECF, because
  ;; they are so simple.
  (load-source "Arithmetic/incf-decf-defmacro.lisp" environment)
  ;; Enable DECLAIM.
  ;; FIXME: define PROCLAIM better.
  (setf (sicl-genv:fdefinition 'proclaim environment)
        (lambda (&rest arguments)
          (format *trace-output* "args: ~s~%" arguments)))
  (sicl-boot:import-function-from-host
   'sicl-evaluation-and-compilation:declaim-expander environment)
  (load-source "Evaluation-and-compilation/declaim-defmacro.lisp" environment)
  ;; Enable OR, AND, WHEN, UNLESS, COND,
  ;; CASE, ECASE, CCASE, TYPECASE, ETYPECASE, CTYPECASE.
  ;; There are no independent expanders for WHEN and UNLESS, because
  ;; they are so simple.
  (sicl-boot:import-functions-from-host
   '(sicl-conditionals:or-expander
     sicl-conditionals:and-expander
     sicl-conditionals:case-expander
     sicl-conditionals:ecase-expander
     sicl-conditionals:ccase-expander
     sicl-conditionals:typecase-expander
     sicl-conditionals:etypecase-expander
     sicl-conditionals:ctypecase-expander)
   environment)
  (load-source "Conditionals/macros.lisp" environment)
  ;; Load a file containing the definition of GET-SETF-EXPANSION.  We
  ;; can't use the host version of this function, because it accesses
  ;; the global environment for SETF expanders.
  (load-source "Data-and-control-flow/get-setf-expansion-defun.lisp" environment)
  ;; Enable PUSH and POP.
  ;; We can't use the host version of the expanders for these macros,
  ;; because they call GET-SETF-EXPANSION at macro expansion time.
  (load-source "Cons/push-pop-support.lisp" environment)
  (load-source "Cons/push-pop-defmacro.lisp" environment)
  ;; Enable WITH-ACCESSORS
  ;; There is no independent expander for WITH-ACCESSORS, because it
  ;; is so simple.
  (load-source "CLOS/with-accessors-defmacro.lisp" environment)
  ;; Enable QUASIQUOTE for Eclector
  (setf (sicl-genv:macro-function 'eclector.reader::quasiquote environment)
        (macro-function 'eclector.reader::quasiquote))
  ;; Enable DEFUN.
  (sicl-boot:import-function-from-host
   'sicl-data-and-control-flow:defun-expander environment)
  (load-source "Data-and-control-flow/defun-defmacro.lisp" environment)
  ;; Enable DEFGENERIC.
  (sicl-boot:import-function-from-host
   'sicl-clos:defgeneric-expander environment)
  (load-source "CLOS/defgeneric-defmacro.lisp" environment)
  ;; Enable DEFCLASS.
  (sicl-boot:import-function-from-host
   'sicl-clos:defclass-expander environment)
  (load-source "CLOS/defclass-defmacro.lisp" environment)
  (load-source "CLOS/find-or-create-generic-function.lisp" environment)
  ;; FIXME: load a file with the definition instead of defining it here.
  (setf (sicl-genv:fdefinition 'sicl-clos::find-accessor-method-class environment)
        (sicl-genv:fdefinition 'sicl-clos::find-class environment))
  ;; Enable DEFMETHOD.
  (sicl-boot:import-function-from-host
   'sicl-clos:defmethod-expander environment)
  (load-source "CLOS/defmethod-defmacro.lisp" environment)
  ;; Enable DEFPARAMETER.
  (load-source "Data-and-control-flow/defparameter-defmacro.lisp" environment)
  ;; Enable DEFVAR.
  (load-source "Data-and-control-flow/defvar-defmacro.lisp" environment)
  ;; Enable DEFCONSTANT.
  (load-source "Data-and-control-flow/defconstant-defmacro.lisp" environment))

(defun enable-compilation (environment)
  (enable-special-operators environment)
  (enable-macros environment))
