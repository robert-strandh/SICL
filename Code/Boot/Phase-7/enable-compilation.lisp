(cl:in-package #:sicl-boot-phase-7)

(defun enable-special-operators (environment)
  (do-symbols (symbol (find-package '#:cleavir-primop))
    (setf (sicl-genv:special-operator symbol environment) t)))

;;; We import most of the expander functions from the host at first.
;;; The reason is that expander function might need some of the
;;; macros, and the dependency might be circular.  So the strategy is
;;; to first import the expanders, then make all the macros work, and
;;; when everything works, load the expanders normally.
(defun enable-macros (environment)
  ;; Enable DECLAIM.
  ;; FIXME: define PROCLAIM better.
  (setf (sicl-genv:fdefinition 'proclaim environment)
        (lambda (&rest arguments)
          (format *trace-output* "args: ~s~%" arguments)))
  (sicl-boot:import-function-from-host
   'sicl-evaluation-and-compilation:declaim-expander environment)
  (sicl-boot:load-source "Evaluation-and-compilation/declaim-defmacro.lisp" environment))

(defun enable-compilation (environment)
  (enable-special-operators environment)
  (enable-macros environment))
