(cl:in-package #:sicl-boot-phase-2)

(defun enable-defmethod (e3 e4)
  (define-generic-function-class-names e4)
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
