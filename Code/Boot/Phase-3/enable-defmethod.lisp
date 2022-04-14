(cl:in-package #:sicl-boot-phase-3)

(defun define-generic-function-class-names (e3)
  (setf (env:fdefinition
         (env:client e3) e3 'sicl-clos::generic-function-class-names)
        (lambda (name environment)
          (declare (ignore environment))
          (let ((function (env:fdefinition (env:client e3) e3 name)))
            (if (or (null function)
                    (not (typep function 'generic-function)))
                (values 'standard-generic-function 'standard-method)
                (values
                 (class-name (class-of function))
                 (class-name
                  (sicl-host-mop:generic-function-method-class function))))))))

(defun define-ensure-method (e1 e2 e3)
  (setf (env:special-variable (env:client e3) e3 'lambda-list-keywords t)
        '(&optional &rest &body &key &allow-other-keys &aux &whole &environment))
  (load-source-file "CLOS/lambda-list-functions.lisp" e3)
  (let ((client (env:client e2)))
    (with-intercepted-function-cells
        (e3
         (typep
          (env:function-cell client e2 'typep))
         (add-method
          (env:function-cell client e2 'add-method))
         (make-instance
          (env:function-cell client e1 'make-instance)))
      (load-source-file "CLOS/ensure-method-defun.lisp" e3))))

(defun enable-defmethod (e1 e2 e3)
  (with-intercepted-function-cells
      (e3
       (find-class
        (list (lambda (name &rest optionals)
                (case name
                  ((t) (find-class 't))
                  (null (find-class 'null))
                  (t (apply (env:fdefinition (env:client e3) e3 'find-class)
                            name optionals)))))))
    (load-source-file "CLOS/make-specializer.lisp" e3))
  (define-generic-function-class-names e3)
  (define-ensure-method e1 e2 e3)
  (let ((client (env:client e2)))
    (setf (env:find-class client e2 't) (find-class 't))
    (setf (env:find-class client e2 'null) (find-class 'null))
    (setf (env:fdefinition client e2 'sicl-clos:method-function)
          #'sicl-host-mop:method-function))
  (load-source-file "CLOS/make-method-lambda-support.lisp" e3)
  (setf (env:macro-function (env:client e3) e3 'sicl-clos:make-method-lambda)
        (lambda (form environment)
          (declare (ignore environment))
          `(sicl-clos::make-method-lambda-default
            nil nil ,(fourth form) nil)))
  (load-source-file "CLOS/defmethod-defmacro.lisp" e3))
