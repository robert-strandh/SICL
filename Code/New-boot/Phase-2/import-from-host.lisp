(cl:in-package #:sicl-new-boot-phase-2)

(defparameter *host-function-names*
  '(;; Evaluation and Compilation
    special-operator-p))

(defun import-host-functions (client global-environment)
  (sb:import-host-functions client global-environment)
  (loop for name in *host-function-names*
        do (setf (clo:fdefinition client global-environment name)
                 (fdefinition name))))

(defparameter *host-setf-functions*
  `(((setf documentation)
     ,(lambda (documentation object documentation-type)
        (setf (documentation object documentation-type) documentation)))))

(defun define-setf-functions (client global-environment)
  (sb:define-setf-functions client global-environment)
  (loop for (name definition) in *host-setf-functions*
        do (setf (clo:fdefinition client global-environment name)
                 definition)))

(defun import-from-host (client global-environment)
  (import-host-functions client global-environment)
  (define-setf-functions client global-environment))
