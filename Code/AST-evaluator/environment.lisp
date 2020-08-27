(cl:in-package #:sicl-ast-evaluator)

(defun make-environment-constellation ()
  (let* ((client (make-instance 'client:sicl))
         (startup-environment
           (make-instance 'env:run-time-environment
             :client client))
         (evaluation-environment
           (make-instance 'env:evaluation-environment
             :parent startup-environment))
         (compilation-environment
           (make-instance 'env:compilation-environment
             :parent evaluation-environment)))
    compilation-environment))

;;; FIXME: We don't want to specialize on a class in a different module.
(defmethod initialize-instance :after
    ((environment env:run-time-environment) &key)
  (let ((client (env:client environment)))
    (do-external-symbols (symbol '#:common-lisp)
      (when (special-operator-p symbol)
        (setf (env:special-operator client environment symbol)
              '(special t))))))

(defvar *run-time-environment-name*)
