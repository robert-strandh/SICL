(cl:in-package #:sicl-hir-interpreter)

(defclass funcallable-standard-object (closer-mop:funcallable-standard-object)
  ((%static-environment :initarg :static-environment :reader static-environment))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod closer-mop:validate-superclass
    ((class funcallable-standard-object)
     (superclass closer-mop:funcallable-standard-object))
  t)

(defun enclose (entry-point code-object &rest static-environment-values)
  (let ((closure (make-instance 'funcallable-standard-object
                   :static-environment
                   (apply #'vector
                    code-object
                    #'enclose
                    #'cons
                    nil
                    static-environment-values))))
    (closer-mop:set-funcallable-instance-function
     closure
     (lambda (&rest args)
       (funcall entry-point
                args
                (static-environment closure)
                sicl-run-time:*dynamic-environment*)))
    closure))

(defun symbol-value-function (global-environment)
  (lambda (symbol)
    (loop for entry in sicl-run-time:*dynamic-environment*
          when (and (typep entry 'sicl-run-time:special-variable-entry)
                    (eq (sicl-run-time:name entry) symbol))
            return (sicl-run-time:value entry)
          finally
             (multiple-value-bind (value boundp)
                 (sicl-genv:special-variable symbol global-environment)
               (if boundp
                   (return value)
                   (multiple-value-bind (value boundp)
                       (sicl-genv:constant-variable symbol global-environment)
                     (if boundp
                         (return value)
                         (error "Unbound variable ~s" symbol))))))))

(defun set-symbol-value-function (global-environment)
  (lambda (value symbol)
    (loop for entry in sicl-run-time:*dynamic-environment*
          when (and (typep entry 'sicl-run-time:special-variable-entry)
                    (eq (sicl-run-time:name entry) symbol))
            do (setf (sicl-run-time:value entry) value)
               (return-from set-symbol-value-function value)
          finally
             ;; FIXME, make sure it is special.
             (setf (sicl-genv:special-variable symbol global-environment t)
                   value)
             (return value))))

(defun fill-environment (environment)
  (setf (sicl-genv:fdefinition 'enclose environment) #'enclose)
  (setf (sicl-genv:fdefinition 'symbol-value environment)
        (symbol-value-function environment))
  (setf (sicl-genv:fdefinition '(setf symbol-value) environment)
        (set-symbol-value-function environment)))
