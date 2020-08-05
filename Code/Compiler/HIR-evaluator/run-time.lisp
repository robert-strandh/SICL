(cl:in-package #:sicl-hir-evaluator)

(defclass uninitialized-closure (closer-mop:funcallable-standard-object)
  ((%entry-point :initarg :entry-point :reader entry-point)
   (%code-object :initarg :code-object :reader code-object))
  (:metaclass closer-mop:funcallable-standard-class))

(defun enclose (entry-point code-object static-environment-length)
  (declare (ignore static-environment-length))
  (let ((closure (make-instance 'uninitialized-closure
                                :entry-point entry-point
                                :code-object code-object)))
    (closer-mop:set-funcallable-instance-function
     closure
     (lambda (&rest args)
       (declare (ignore args))
       (error "Uninitialized closure ~S called" closure)))
    closure))

(defun initialize-closure (closure &rest static-environment-values)
  (check-type closure uninitialized-closure)
  (let* ((entry-point (entry-point closure))
         (code-object (code-object closure))
         (static-environment
           (apply #'vector
                  code-object
                  #'enclose
                  #'initialize-closure
                  #'cons
                  nil
                  static-environment-values)))
    (change-class closure 'closer-mop:funcallable-standard-object)
    (closer-mop:set-funcallable-instance-function
     closure
     (lambda (&rest args)
       (funcall entry-point
                args
                static-environment
                sicl-run-time:*dynamic-environment*)))))

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
  (setf (sicl-genv:fdefinition 'enclose environment)
        #'enclose)
  (setf (sicl-genv:fdefinition 'initialize-closure environment)
        #'initialize-closure)
  (setf (sicl-genv:fdefinition 'symbol-value environment)
        (symbol-value-function environment))
  (setf (sicl-genv:fdefinition '(setf symbol-value) environment)
        (set-symbol-value-function environment)))
