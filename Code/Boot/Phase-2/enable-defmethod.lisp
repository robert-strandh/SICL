(cl:in-package #:sicl-boot-phase-2)

(defun define-generic-function-class-names (e3)
  (setf (env:fdefinition
         (env:client e3) e3 'sicl-clos::generic-function-class-names)
        (lambda (name environment)
          (declare (ignore environment))
          (let ((client (env:client e3)))
            (if (and (env:fboundp client e3 name)
                     (or (consp name)
                         (and (null (env:macro-function client e3 name))
                              (not (env:special-operator client e3 name)))))
                (let ((function (env:fdefinition (env:client e3) e3 name)))
                  (if (typep function 'generic-function)
                      (values
                       (class-name (class-of function))
                       (class-name
                        (closer-mop:generic-function-method-class function)))
                      (values 'standard-generic-function 'standard-method)))
                (values 'standard-generic-function 'standard-method))))))

(defun define-ensure-method (e1 e2 e3)
  (setf (env:special-variable (env:client e3) e3 'lambda-list-keywords t)
        '(&optional &reest &body &key &allow-other-keys &aux &whole &environment))
  (load-source-file "CLOS/lambda-list-functions.lisp" e3)
  (let ((client (env:client e2)))
    (with-intercepted-function-cells
        (e3
         (typep
          (env:function-cell client e2 'typep))
         (add-method
          (env:function-cell client e2 'add-method))
         (find-class
          (list (lambda (name &rest optionals)
                  (case name
                    ((t) (find-class 't))
                    (null (find-class 'null))
                    (t (apply (env:fdefinition (env:client e3) e3 'find-class)
                              name optionals))))))
         (make-instance
          (env:function-cell client e1 'make-instance)))
      (load-source-file "CLOS/ensure-method-defun.lisp" e3))))

(defun enable-defmethod (e1 e2 e3)
  (define-generic-function-class-names e3)
  (define-ensure-method e1 e2 e3)
  (let ((client (env:client e2)))
    (setf (env:find-class client e2 't) (find-class 't))
    (setf (env:find-class client e2 'null) (find-class 'null))
    (setf (env:fdefinition client e2 'sicl-clos:method-function)
          #'closer-mop:method-function)
    (setf (env:special-operator client e3 'cleavir-primop:multiple-value-call) t))
  (import-functions-from-host
   '(sicl-clos:parse-defmethod sicl-clos:canonicalize-specializers
     (setf env:macro-function))
   e3)
  (load-source-file "CLOS/make-method-lambda-support.lisp" e3)
  (setf (env:macro-function (env:client e3) e3 'sicl-clos:make-method-lambda)
        (lambda (form environment)
          (declare (ignore environment))
          `(sicl-clos::make-method-lambda-default
            nil nil ,(fourth form) nil)))
  (load-source-file "CLOS/defmethod-defmacro.lisp" e3))
