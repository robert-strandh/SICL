(cl:in-package #:sicl-boot-phase-4)

(defun canonicalize-specializers (specializers e4)
  (loop for specializer in specializers
        collect (cond ((symbolp specializer)
                       (env:find-class (env:client e4) e4 specializer))
                      (t
                       specializer))))

(defun define-ensure-method (e3 e4 e5)
  (setf (env:fdefinition (env:client e5) e5 'sicl-clos:ensure-method)
        (lambda (name
                 &key
                   method-class
                   lambda-list
                   qualifiers
                   specializers
                   function
                   (slot-definition nil slot-definition-p)
                 &allow-other-keys)
          (assert (or (symbolp name) (consp name)))
          (let* ((ensure-gf (env:fdefinition (env:client e5) e5 'ensure-generic-function))
                 (add-method (env:fdefinition (env:client e4) e4 'add-method))
                 (make-instance (env:fdefinition (env:client e3) e3 'make-instance))
                 (method-class
                   (if (symbolp method-class)
                       (env:find-class (env:client e3) e3 method-class)
                       method-class))
                 (generic-function
                   (funcall ensure-gf name
                            :lambda-list lambda-list
                            :generic-function-class 'standard-generic-function
                            :method-class method-class))
                 (method
                   (if slot-definition-p
                       (funcall
                        make-instance method-class
                        :lambda-list lambda-list
                        :qualifiers qualifiers
                        :specializers (canonicalize-specializers specializers e4)
                        :slot-definition slot-definition
                        :function function)
                       (funcall
                        make-instance
                        method-class
                        :lambda-list lambda-list
                        :qualifiers qualifiers
                        :specializers (canonicalize-specializers specializers e4)
                        :function function))))
            (funcall add-method generic-function method)
            method))))

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
       (find-class
        (list (lambda (name)
                (assert (eq name 't))
                (env:find-class (env:client e4) e4 't))))
       (slot-boundp
        (env:function-cell (env:client e3) e3 'slot-boundp)))
    (load-source-file "CLOS/add-remove-method-support.lisp" e4))
  (load-source-file "CLOS/add-remove-method-defmethods.lisp" e4)
  (load-source-file "CLOS/defmethod-support.lisp" e5)
  (load-source-file "CLOS/defmethod-defmacro.lisp" e5))
