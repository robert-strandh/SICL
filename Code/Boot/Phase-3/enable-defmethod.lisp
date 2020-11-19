(cl:in-package #:sicl-boot-phase-3)

(defun canonicalize-specializers (specializers e3)
  (loop for specializer in specializers
        collect (cond ((symbolp specializer)
                       (env:find-class (env:client e3) e3 specializer))
                      (t
                       specializer))))

(defun define-ensure-method (e2 e3 e4)
  (setf (env:fdefinition (env:client e4) e4 'sicl-clos:ensure-method)
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
          (assert (symbolp method-class))
          (let* ((ensure-gf (env:fdefinition (env:client e4) e4 'ensure-generic-function))
                 (add-method (env:fdefinition (env:client e3) e3 'add-method))
                 (generic-function
                   (funcall ensure-gf name
                            :lambda-list lambda-list
                            :generic-function-class 'standard-generic-function
                            :method-class method-class))
                 (method
                   (if slot-definition-p
                       (make-instance
                           (env:find-class (env:client e2) e2 method-class)
                         :lambda-list lambda-list
                         :qualifiers qualifiers
                         :specializers (canonicalize-specializers specializers e3)
                         :slot-definition slot-definition
                         :function function)
                       (make-instance
                           (env:find-class (env:client e2) e2 method-class)
                         :lambda-list lambda-list
                         :qualifiers qualifiers
                         :specializers (canonicalize-specializers specializers e3)
                         :function function))))
            (funcall add-method generic-function method)
            method))))

(defun enable-defmethod (e2 e3 e4)
  (define-generic-function-class-names e4)
  (define-ensure-method e2 e3 e4)
  (load-source-file "CLOS/make-method-lambda-support.lisp" e4)
  (setf (env:macro-function (env:client e4) e4 'sicl-clos:make-method-lambda)
        (lambda (form environment)
          (declare (ignore environment))
          `(sicl-clos::make-method-lambda-default
            nil nil ,(fourth form) nil)))
  (import-functions-from-host
   '(cleavir-code-utilities:parse-specialized-lambda-list
     cleavir-code-utilities:separate-function-body
     cleavir-code-utilities:required)
   e4)
  (load-source-file "CLOS/add-remove-direct-method-defgenerics.lisp" e3)
  (load-source-file "CLOS/add-remove-direct-method-support.lisp" e3)
  (load-source-file "CLOS/add-remove-direct-method-defmethods.lisp" e3)
  (load-source-file "CLOS/dependent-maintenance-defgenerics.lisp" e3)
  (load-source-file "CLOS/dependent-maintenance-support.lisp" e3)
  (load-source-file "CLOS/dependent-maintenance-defmethods.lisp" e3)
  (load-source-file "CLOS/add-remove-method-defgenerics.lisp" e3)
  (with-intercepted-function-cells
      (e3
       (find-class
        (list (lambda (name)
                (assert (eq name 't))
                (env:find-class (env:client e3) e3 't))))
       (slot-boundp (list #'slot-boundp)))
    (load-source-file "CLOS/add-remove-method-support.lisp" e3))
  (load-source-file "CLOS/add-remove-method-defmethods.lisp" e3)
  (load-source-file "CLOS/defmethod-support.lisp" e4)
  (load-source-file "CLOS/defmethod-defmacro.lisp" e4))
