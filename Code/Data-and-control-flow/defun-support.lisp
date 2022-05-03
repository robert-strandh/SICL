(cl:in-package #:sicl-data-and-control-flow)

;;; FIXME: Do more syntax verification.

(defun defun-expander (name lambda-list body environment)
  (multiple-value-bind (declarations documentation forms)
      (cleavir-code-utilities:separate-function-body body)
    (let ((env-var (gensym)))
      `(progn
         (eval-when (:compile-toplevel)
           (let* ((,env-var (env:global-environment ,environment)))
             (setf (env:function-description
                    ,env-var ',name)
                   (env:make-simple-function-description
                    ',lambda-list))))
         (eval-when (:load-toplevel :execute)
           (setf (fdefinition ',name)
                 (lambda ,lambda-list
                   ,@declarations
                   ,@(if (null documentation)
                         '()
                         (list documentation))
                   (block ,(if (symbolp name) name (second name))
                     ,@forms)))
           ',name)))))
