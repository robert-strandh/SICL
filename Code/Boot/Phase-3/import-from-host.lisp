(cl:in-package #:sicl-boot-phase-3)

(defun import-misc (e3)
  (import-functions-from-host
   '(slot-unbound
     no-applicable-method
     sicl-method-combination:define-method-combination-expander
     shared-initialize initialize-instance reinitialize-instance
     closer-mop:method-function
     sicl-clos:parse-defmethod sicl-clos:canonicalize-specializers
     (setf env:macro-function)
     coerce)
   e3)
  (setf (env:macro-function (env:client e3) e3 'defpackage)
        (lambda (form env)
          (declare (ignore env))
          (eval form)
          nil)))

(defun import-from-host (e3)
  (import-misc e3))
