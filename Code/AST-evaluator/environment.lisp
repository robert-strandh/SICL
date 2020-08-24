(cl:in-package #:sicl-ast-evaluator)

(defclass base-run-time-environment
    (clostrum/virtual::virtual-run-time-environment)
  ())

(defclass run-time-environment
    (base-run-time-environment)
  ((%client :initarg :client :reader client)))

(defclass evaluation-environment
    (base-run-time-environment
     clostrum:evaluation-environment-mixin)
  ())

(defclass compilation-environment
    (clostrum:compilation-environment)
  ())

(defmethod client ((environment compilation-environment))
  (client (env:parent environment)))

(defmethod client ((environment evaluation-environment))
  (client (env:parent environment)))

(defun make-environment-constellation ()
  (let* ((client (make-instance 'client))
         (startup-environment
           (make-instance 'run-time-environment
             :client client))
         (evaluation-environment
           (make-instance 'evaluation-environment
             :parent startup-environment))
         (compilation-environment
           (make-instance 'compilation-environment
             :parent evaluation-environment)))
    compilation-environment))

(defmethod initialize-instance :after
    ((environment run-time-environment) &key)
  (let ((client (client environment)))
    (do-external-symbols (symbol '#:common-lisp)
      (when (special-operator-p symbol)
        (setf (env:special-operator client environment symbol)
              '(special t))))))

(defvar *run-time-environment-name*)
