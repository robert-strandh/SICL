(cl:in-package #:sicl-boot-phase-5)

(defun define-ast-eval (e5)
  (setf (env:fdefinition (env:client e5) e5 'sicl-boot:ast-eval)
        (lambda (client ast)
          (sicl-boot-compile-and-tie:compile-and-tie
           client e5 ast))))

(defun finalize-classes (e3 e4)
  (format *trace-output* "Finalizing all classes in ~a..." (sicl-boot:name e4))
  (finish-output *trace-output*)
  (let ((finalized-p (env:fdefinition (env:client e3) e3 'sicl-clos::class-finalized-p))
        (finalize (env:fdefinition (env:client e3) e3 'sicl-clos:finalize-inheritance)))
    (env:map-defined-classes
     (env:client e4) e4
     (lambda (name class)
       (declare (ignore name))
       (unless (funcall finalized-p class)
         (funcall finalize class)))))
  (format *trace-output* "done~%")
  (finish-output *trace-output*))

(defun prepare-this-phase (e3 e4 e5)
  (define-ast-eval e5)
  (sicl-boot:copy-macro-functions e4 e5)
  (ensure-asdf-system '#:sicl-data-and-control-flow-type-proclamations e5)
  (ensure-asdf-system '#:sicl-cons-type-proclamations e5)
  (ensure-asdf-system '#:sicl-arithmetic-type-proclamations e5)
  (ensure-asdf-system '#:sicl-conditions-type-proclamations e5)
  (ensure-asdf-system '#:sicl-symbol-type-proclamations e5)
  (ensure-asdf-system '#:sicl-type-type-proclamations e5)
  (ensure-asdf-system '#:sicl-character-type-proclamations e5)
  (ensure-asdf-system '#:sicl-sequence-type-proclamations e5)
  (ensure-asdf-system '#:sicl-run-time-type-proclamations e5)
  (ensure-asdf-system '#:sicl-clos-type-proclamations e5)
  (ensure-asdf-system '#:sicl-string-type-proclamations e5)
  (ensure-asdf-system '#:cleavir-code-utilities-type-proclamations e5)
  (ensure-asdf-system '#:sicl-array-proclamations e5)
  (load-source-file "CLOS/class-of-defun.lisp" e4)
  (enable-typep e3 e4)
  (load-source-file "Types/type-of-defgeneric.lisp" e4)
  (enable-object-creation e3 e4)
  (import-functions-from-host
   '(intern
     ;; Eclector uses EVAL in some compiler macros to evaluate some
     ;; Boolean arguments, but it is applied only to contstants so we
     ;; can use the host EVAL.
     eval)
   e5)
  (enable-method-combinations e3 e4 e5)
  (setf (env:fdefinition (env:client e4) e4 'compile)
        (lambda (x lambda-expression)
          (assert (null x))
          (assert (and (consp lambda-expression) (eq (first lambda-expression) 'lambda)))
          (with-intercepted-function-cells
              (e4
               (make-instance
                   (env:function-cell (env:client e3) e3 'make-instance)))
            (let* ((cst (cst:cst-from-expression lambda-expression))
                   (ast (sicl-boot::cst-to-ast (env:client e4) e4 cst nil)))
              (sicl-boot:ast-eval (env:client e4) e4 ast)))))
  (enable-compute-discriminating-function e3 e4 e5)
  (load-source-file "CLOS/defgeneric-support.lisp" e5)
  (with-intercepted-function-cells
      (e4
       (sicl-clos:set-funcallable-instance-function
        (list #'closer-mop:set-funcallable-instance-function)))
    (load-source-file "CLOS/invalidate-discriminating-function.lisp" e4))
  (load-source-file "CLOS/generic-function-initialization-support.lisp" e4)
  (with-intercepted-function-cells
      (e4
       (sicl-clos:method-function
        (env:function-cell (env:client e3) e3 'sicl-clos:method-function)))
    (load-source-file "CLOS/generic-function-initialization-defmethods.lisp" e4))
  (enable-defgeneric e3 e4 e5)
  (enable-defmethod e3 e4 e5)
  (enable-defclass e3 e4 e5)
  (finalize-classes e3 e4))
