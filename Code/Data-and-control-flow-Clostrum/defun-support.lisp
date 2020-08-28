(cl:in-package #:sicl-data-and-control-flow)

;;; FIXME: Do more syntax verification.

(defun defun-expander (name lambda-list body environment)
  (multiple-value-bind (declarations documentation forms)
      (cleavir-code-utilities:separate-function-body body)
    (let ((env-var (gensym))
          (client-var (gensym)))
      `(progn
         (eval-when (:compile-toplevel)
           (let* ((,env-var (sicl-environment:global-environment ,environment))
                  (,client-var (sicl-environment:client ,env-var)))
             (setf (sicl-environment:function-description ,client-var ,env-var ',name)
                   (make-instance 'sicl-environment:simple-function-description
                     :lambda list ',lambda-list))))
         (eval-when (:load-toplevel :execute)
           (let* ((,env-var (sicl-environment:global-environment))
                  (,client-var (sicl-environment:client ,env-var)))
             (setf (sicl-environment:fdefinition ,client-var ,env-var ',name)
                   (lambda ,lambda-list
                     ,@declarations
                     ,@(if (null documentation)
                           '()
                           (list documentation))
                     (block ,(if (symbolp name) name (second name))
                       ,@forms))))
           ',name)))))
