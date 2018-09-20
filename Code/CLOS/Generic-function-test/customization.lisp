(cl:in-package #:sicl-clos)

(defparameter *env* (make-instance 'sicl-simple-environment:simple-environment))

(defmethod sicl-genv:find-method-combination-template (name (env null))
  (sicl-genv:find-method-combination-template name *env*))

(defmethod (setf sicl-genv:find-method-combination-template) (mc name (env null))
  (setf (sicl-genv:find-method-combination-template name *env*) mc))
