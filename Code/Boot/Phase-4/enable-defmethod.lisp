(cl:in-package #:sicl-boot-phase-4)

(defun define-generic-function-class-names (e4)
  (setf (env:fdefinition
         (env:client e4) e4 'sicl-clos::generic-function-class-names)
        (lambda (name environment)
          (declare (ignore name environment))
          (values 'standard-generic-function 'standard-method))))

(defun define-ensure-method (e2 e3 e4)
  (setf (env:special-variable (env:client e4) e4 'lambda-list-keywords t)
        '(&optional &rest &body &key &allow-other-keys &aux &whole &environment))
  (load-source-file "CLOS/lambda-list-functions.lisp" e4)
  (let ((client (env:client e3)))
    (with-intercepted-function-cells
        (e4
         (typep
          (env:function-cell client e3 'typep))
         (add-method
          (env:function-cell client e3 'add-method))
         (make-instance
          (env:function-cell client e2 'make-instance)))
      (load-source-file "CLOS/ensure-method-defun.lisp" e4))))

(defun enable-defmethod (e2 e3 e4)
  (define-generic-function-class-names e4)
  (define-ensure-method e2 e3 e4)
  (load-source-file "CLOS/make-method-lambda-support.lisp" e4)
  (setf (env:macro-function (env:client e4) e4 'sicl-clos:make-method-lambda)
        (lambda (form environment)
          (declare (ignore environment))
          `(sicl-clos::make-method-lambda-default
            nil nil ,(fourth form) nil)))
  (load-source-file "CLOS/add-remove-direct-method-defgenerics.lisp" e3)
  (load-source-file "CLOS/add-remove-direct-method-support.lisp" e3)
  (load-source-file "CLOS/add-remove-direct-method-defmethods.lisp" e3)
  (load-source-file "CLOS/dependent-maintenance-defgenerics.lisp" e3)
  (load-source-file "CLOS/dependent-maintenance-support.lisp" e3)
  (load-source-file "CLOS/dependent-maintenance-defmethods.lisp" e3)
  (load-source-file "CLOS/add-remove-method-defgenerics.lisp" e3)
  (with-intercepted-function-cells
      (e3
       (find-class (env:function-cell (env:client e4) e4 'find-class))
       (slot-boundp (list #'slot-boundp)))
    (load-source-file "CLOS/add-remove-method-support.lisp" e3))
  (load-source-file "CLOS/add-remove-method-defmethods.lisp" e3)
  (load-source-file "CLOS/defmethod-support.lisp" e4)
  (load-source-file "CLOS/defmethod-defmacro.lisp" e4))
