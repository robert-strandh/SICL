(cl:in-package #:sicl-hir-evaluator)

(defclass hir-closure (sicl-host-mop:funcallable-standard-object)
  ((%environment :initarg :environment :reader environment))
  (:metaclass sicl-host-mop:funcallable-standard-class))

(defun enclose (entry-point static-environment-length lexical-locations)
  (let* ((static-environment
           (make-array static-environment-length))
         (closure (make-instance 'hir-closure
                    :environment static-environment)))
    (sicl-host-mop:set-funcallable-instance-function
     closure
     (lambda (&rest args)
       (funcall entry-point
                args
                static-environment
                sicl-run-time:*dynamic-environment*
                lexical-locations)))
    closure))

(defun initialize-closure (closure &rest static-environment-values)
  (check-type closure hir-closure)
  (let ((static-environment (environment closure)))
    (declare (simple-vector static-environment))
    (replace static-environment static-environment-values)))

(defun fill-environment (environment)
  (let ((client (env:client environment)))
    (setf (env:fdefinition client environment 'enclose)
          #'enclose)
    (setf (env:fdefinition client environment 'initialize-closure)
          #'initialize-closure)))
