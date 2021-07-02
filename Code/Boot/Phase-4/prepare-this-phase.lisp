(cl:in-package #:sicl-boot-phase-4)

(defun finalize-classes (e3)
  (format *trace-output* "Finalizing all classes in E3...")
  (finish-output *trace-output*)
  (env:map-defined-classes
   (env:client e3) e3
   (lambda (name class)
     (unless (eq name 'symbol)
       (sicl-host-mop:finalize-inheritance class))))
  (format *trace-output* "done!~%"))

(defun prepare-this-phase (e2 e3 e4)
  (setf (env:fdefinition (env:client e4) e4 'sicl-boot:ast-eval)
        (lambda (ast)
          (sicl-ast-evaluator:eval-ast ast e4)))
  (sicl-boot:copy-macro-functions e3 e4)
  (load-source-file "CLOS/class-of-defun.lisp" e3)
  (enable-typep e2 e3)
  (load-source-file "Types/type-of-defgeneric.lisp" e3)
  (enable-object-creation e2 e3)
  (enable-method-combinations e2 e3 e4)
  (setf (env:special-operator (env:client e4) e4 'cleavir-primop:multiple-value-call) t)
  (enable-compute-discriminating-function e2 e3 e4)
  (load-source-file "CLOS/defgeneric-support.lisp" e4)
  (load-source-file "CLOS/invalidate-discriminating-function.lisp" e3)
  ;; These two definitions exist so that COMPILE will not signal an
  ;; error that they are undefined.
  (setf (env:fdefinition (env:client e3) e3 'slot-value)
        (lambda (object slot-name)
          (error "SLOT-VALUE called with ~s and ~s" object slot-name)))
  (setf (env:fdefinition (env:client e3) e3 '(setf slot-value))
        (lambda (value object slot-name)
          (error "(SETF SLOT-VALUE) called with ~s, ~s and ~s" value object slot-name)))
  (load-source-file "CLOS/generic-function-initialization-support.lisp" e3)
  (with-intercepted-function-cells
      (e3
       (sicl-clos:method-function
        (env:function-cell (env:client e2) e2 'sicl-clos:method-function)))
    (load-source-file "CLOS/generic-function-initialization-defmethods.lisp" e3))
  (enable-defgeneric e2 e3 e4)
  (enable-defmethod e2 e3 e4)
  (enable-defclass e2 e3 e4)
  (finalize-classes e3))
