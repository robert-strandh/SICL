(cl:in-package #:sicl-hir-to-cl)

;;; Most of the code in this file should be moved to the boot system
;;; once everything is working.

(defgeneric static-environment (function))

(defclass funcallable-standard-object (closer-mop:funcallable-standard-object)
  ((%entry-point :initarg :entry-point :reader entry-point)
   (%static-environment :initarg :static-environment :reader static-environment))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod closer-mop:validate-superclass
    ((class funcallable-standard-object)
     (superclass closer-mop:funcallable-standard-object))
  t)

(defun enclose (entry-point code-object &rest static-environment-values)
  (let ((closure (make-instance 'funcallable-standard-object
                   :entry-point entry-point
                   :static-environment
                   (apply #'vector
                    code-object
                    #'enclose
                    #'cons
                    nil
                    static-environment-values))))
    closure))

(defun make-function-cell-finder (environment)
  (lambda (name)
    (sicl-genv:function-cell name environment)))

(defun symbol-value-function (environment)
  (lambda (symbol)
    (loop for entry in *dynamic-environment*
          when (and (typep entry 'special-variable-entry)
                    (eq (name entry) symbol))
            return (value entry)
          finally
             (multiple-value-bind (value boundp)
                 (sicl-genv:special-variable symbol environment)
               (if boundp
                   (return value)
                   (multiple-value-bind (value boundp)
                       (sicl-genv:constant-variable symbol environment)
                     (if boundp
                         (return value)
                         (error "Unbound variable ~s" symbol))))))))

(defun set-symbol-value-function (environment)
  (lambda (value symbol)
    (loop for entry in *dynamic-environment*
          when (and (typep entry 'special-variable-entry)
                    (eq (name entry) symbol))
            do (setf (value entry) value)
               (return-from set-symbol-value-function value)
          finally
             ;; FIXME, make sure it is special.
             (setf (sicl-genv:special-variable symbol environment t)
                   value)
             (return value))))

(defun fill-environment (environment)
  (setf (sicl-genv:fdefinition 'enclose environment) #'enclose)
  (setf (sicl-genv:fdefinition 'static-environment-function environment)
        #'static-environment)
  (setf (sicl-genv:fdefinition 'symbol-value environment)
        (symbol-value-function environment))
  (setf (sicl-genv:fdefinition '(setf symbol-value) environment)
        (set-symbol-value-function environment)))
