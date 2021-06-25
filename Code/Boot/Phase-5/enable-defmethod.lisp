(cl:in-package #:sicl-boot-phase-4)

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
  (define-generic-function-class-names e5)
  (define-ensure-method e3 e4 e5)
  (load-source-file "CLOS/make-method-lambda-support.lisp" e5)
  (setf (env:macro-function (env:client e5) e5 'sicl-clos:make-method-lambda)
        (lambda (form environment)
          (declare (ignore environment))
          `(sicl-clos::make-method-lambda-default
            nil nil ,(fourth form) nil)))
  (import-functions-from-host
   '(cleavir-code-utilities:parse-specialized-lambda-list
     cleavir-code-utilities:separate-function-body
     cleavir-code-utilities:required)
   e5)
  (load-source-file "CLOS/add-remove-direct-method-defgenerics.lisp" e4)
  (load-source-file "CLOS/add-remove-direct-method-support.lisp" e4)
  (load-source-file "CLOS/add-remove-direct-method-defmethods.lisp" e4)
  (load-source-file "CLOS/dependent-maintenance-defgenerics.lisp" e4)
  (load-source-file "CLOS/dependent-maintenance-support.lisp" e4)
  (load-source-file "CLOS/dependent-maintenance-defmethods.lisp" e4)
  (load-source-file "CLOS/add-remove-method-defgenerics.lisp" e4)
  (with-intercepted-function-cells
      (e4
       (find-class (env:function-cell (env:client e5) e5 'find-class))
       (env:function-cell (env:client e3) e3 'slot-boundp))
    (load-source-file "CLOS/add-remove-method-support.lisp" e4))
  (load-source-file "CLOS/add-remove-method-defmethods.lisp" e4)
  (load-source-file "CLOS/defmethod-support.lisp" e5)
  (load-source-file "CLOS/defmethod-defmacro.lisp" e5))
