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
  ;; Enable LAMBDA.
  ;; There is no independent expander for LAMBDA, because it is so
  ;; simple.
  (sicl-boot:load-source "Evaluation-and-compilation/lambda.lisp" environment)
  ;; Enable DECLAIM.
  ;; FIXME: define PROCLAIM better.
  (setf (sicl-genv:fdefinition 'proclaim environment)
        (lambda (&rest arguments)
          (format *trace-output* "args: ~s~%" arguments)))
  (sicl-boot:import-function-from-host
   'sicl-evaluation-and-compilation:declaim-expander environment)
  (sicl-boot:load-source "Evaluation-and-compilation/declaim-defmacro.lisp" environment)
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
  (sicl-boot:load-source "Conditionals/macros.lisp" environment)
  ;; Enable DEFGENERIC.
  (sicl-boot:import-function-from-host
   'sicl-clos:defgeneric-expander environment)
  (sicl-boot:load-source "CLOS/defgeneric-defmacro.lisp" environment)
  ;; Enable DEFCLASS.
  (sicl-boot:import-function-from-host
   'sicl-clos:defclass-expander environment)
  (sicl-boot:load-source "CLOS/defclass-defmacro.lisp" environment)
  ;; Enable DEFMETHOD.
  (sicl-boot:import-function-from-host
   'sicl-clos:defmethod-expander environment)
  (sicl-boot:load-source "CLOS/defmethod-defmacro.lisp" environment))

(defun enable-compilation (environment)
  (enable-special-operators environment)
  (enable-macros environment))
