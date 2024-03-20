(cl:in-package #:sicl-new-boot-phase-1)

(defparameter *host-function-names*
  '(;; Objects
    initialize-instance reinitialize-instance shared-initialize
    slot-boundp slot-value class-of))

(defun import-host-functions (client global-environment)
  (sb:import-host-functions client global-environment)
  (loop for name in *host-function-names*
        do (setf (clo:fdefinition client global-environment name)
                 (fdefinition name))))

(defun import-host-classes (client global-environment)
  (loop for name in '(symbol)
        do (setf (clo:find-class client global-environment name)
                 (find-class 't))))

(defparameter *host-setf-functions*
  `(((setf gethash)
     ,(lambda (object key table)
        (setf (gethash key table) object)))
    ((setf slot-value)
     ,(lambda (new-value object slot-name)
        (setf (slot-value object slot-name) new-value)))
    ((setf documentation)
     ,(lambda (documentation object documentation-type)
        (setf (documentation object documentation-type) documentation)))))

(defun define-setf-functions (client global-environment)
  (sb:define-setf-functions client global-environment)
  (loop for (name definition) in *host-setf-functions*
        do (setf (clo:fdefinition client global-environment name)
                 definition)))

(defun import-from-host (client global-environment)
  (import-host-functions client global-environment)
  (import-host-classes client global-environment)
  (define-setf-functions client global-environment))
