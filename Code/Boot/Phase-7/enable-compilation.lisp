(cl:in-package #:sicl-boot-phase-7)

(defun enable-special-operators (environment)
  (do-symbols (symbol (find-package '#:cleavir-primop))
    (setf (sicl-genv:special-operator symbol environment) t)))

;;; We import most of the expander functions from the host at first.
;;; The reason is that expander function might need some of the
;;; macros, and the dependency might be circular.  So the strategy is
;;; to first import the expanders, then make all the macros work, and
;;; when everything works, load the expanders normally.  It would seem
;;; that we could load the FASLs of the expander functions, compiled
;;; in environment E0, but some macros have temporary, incorrect
;;; definitions in E0, and in particular the DEFGENERIC macro.  So if
;;; a file defining an expander function contains a call to any of
;;; those macros with temporary definitions, then it will be
;;; incorrectly compiled.
(defun enable-macros (environment)
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
  (sicl-boot:import-function-from-host
   'sicl-clos:defgeneric-expander environment)
  (sicl-boot:load-source "CLOS/defgeneric-defmacro.lisp" environment))

(defun enable-compilation (environment)
  (enable-special-operators environment)
  (enable-macros environment))
