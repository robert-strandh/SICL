(cl:in-package #:sicl-hir-evaluator)

(defclass hir-closure (closer-mop:funcallable-standard-object)
  ((%environment :initarg :environment :reader environment))
  (:metaclass closer-mop:funcallable-standard-class))

(defun enclose (entry-point code-object static-environment-length lexical-locations)
  (let* ((static-environment
           (make-array (+ sicl-compiler:+first-constant-index+ static-environment-length)))
         (closure (make-instance 'hir-closure
                    :environment static-environment)))
    (setf (svref static-environment sicl-compiler:+code-object-index+)
          code-object)
    (setf (svref static-environment sicl-compiler:+cons-function-index+)
          #'cons)
    (setf (svref static-environment sicl-compiler:+nil-index+)
          nil)
    (closer-mop:set-funcallable-instance-function
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
    (replace static-environment static-environment-values
             :start1 sicl-compiler:+first-constant-index+)))

(defun fill-environment (environment)
  (let ((client (env:client environment)))
    (setf (env:fdefinition client environment 'enclose)
          #'enclose)
    (setf (env:fdefinition client environment 'initialize-closure)
          #'initialize-closure)))
