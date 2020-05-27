(cl:in-package #:sicl-boot-phase-7)

(defun enable-special-operators (environment)
  (do-symbols (symbol (find-package '#:cleavir-primop))
    (setf (sicl-genv:special-operator symbol environment) t)))

(defun enable-compilation (environment)
  (enable-special-operators environment))
