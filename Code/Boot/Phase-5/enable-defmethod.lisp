(cl:in-package #:sicl-boot-phase-5)

(defun define-generic-function-class-names (e5)
  (setf (env:fdefinition
         (env:client e5) e5 'sicl-clos::generic-function-class-names)
        (lambda (name environment)
          (declare (ignore name environment))
          (values 'standard-generic-function 'standard-method))))

(defun define-ensure-method (e3 e4 e5)
  (setf (env:special-variable (env:client e5) e5 'lambda-list-keywords t)
        '(&optional &rest &body &key &allow-other-keys &aux &whole &environment))
  (load-source-file "CLOS/lambda-list-functions.lisp" e5)
  (let ((client (env:client e4)))
    (with-intercepted-function-cells
        (e5
         (typep
          (env:function-cell client e4 'typep))
         (add-method
          (env:function-cell client e4 'add-method))
         (make-instance
          (env:function-cell client e3 'make-instance)))
      (load-source-file "CLOS/ensure-method-defun.lisp" e5))))

(defun enable-defmethod (e3 e4 e5)
  (load-source-file "CLOS/make-specializer.lisp" e5)
  (define-generic-function-class-names e5)
  (define-ensure-method e3 e4 e5)
  (load-source-file "CLOS/make-method-lambda-support.lisp" e5)
  (setf (env:macro-function (env:client e5) e5 'sicl-clos:make-method-lambda)
        (lambda (form environment)
          (declare (ignore environment))
          `(sicl-clos::make-method-lambda-default
            nil nil ,(fourth form) nil)))
  (load-source-file "CLOS/add-remove-direct-method.lisp" e4)
  (load-source-file "CLOS/dependent-maintenance.lisp" e4)
  (with-intercepted-function-cells
      (e4
       (find-class (env:function-cell (env:client e5) e5 'find-class))
       (env:function-cell (env:client e3) e3 'slot-boundp))
    (load-source-file "CLOS/add-remove-method.lisp" e4))
  (load-source-file "CLOS/defmethod-support.lisp" e5)
  (load-source-file "CLOS/defmethod-defmacro.lisp" e5))
