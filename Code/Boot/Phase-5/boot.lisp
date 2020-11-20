(cl:in-package #:sicl-boot-phase-5)

(defun ast-eval (ast client environment)
  (let* ((global-environment (trucler:global-environment client environment))
         (hir (sicl-ast-to-hir:ast-to-hir client ast))
         (fun (sicl-hir-evaluator:top-level-hir-to-host-function client hir))
         (sicl-run-time:*dynamic-environment* '())
         (function-cell-function
           (sicl-environment:fdefinition
            client global-environment 'sicl-data-and-control-flow:function-cell)))
    (funcall fun
             (apply #'vector
                    nil ; Ultimately, replace with code object.
                    #'sicl-hir-evaluator:enclose
                    #'sicl-hir-evaluator:initialize-closure
                    #'cons
                    nil
                    (append (loop with names = (sicl-hir-transformations:function-names hir)
                                  for name in names
                                  collect (funcall function-cell-function name))
                            (sicl-hir-transformations:constants hir))))))

(defun finalize-inheritance (e4)
  (format *trace-output*
          "Finalizing all classes in ~a..."
          (sicl-boot:name e4))
  (finish-output *trace-output*)
  (let ((fun (env:fdefinition (env:client e4) e4 'sicl-clos:finalize-inheritance)))
    (do-all-symbols (symbol)
      (let ((class (env:find-class (env:client e4) e4 symbol)))
        (unless (or (null class)
                    (funcall (env:fdefinition (env:client e4) e4 'sicl-clos::class-finalized-p)
                             class))
          (funcall fun class)))))
  (format *trace-output* "done~%")
  (finish-output *trace-output*))

(defun boot (boot)
  (format *trace-output* "Start phase 5~%")
  (with-accessors ((e0 sicl-boot:e0)
                   (e3 sicl-boot:e3)
                   (e4 sicl-boot:e4)
                   (e5 sicl-boot:e5))
      boot
    (change-class e5 'environment :client (make-instance 'client))
    (with-intercepted-function-cells
        (e5
         (find-class
          (list (lambda (name)
                  (env:find-class (env:client e4) e4 name)))))
      (sicl-boot:create-accessor-defgenerics e5)
      (sicl-boot:create-mop-classes e5)
      (with-intercepted-function-cells
          (e4
           (find-class
            (list (lambda (name)
                    (env:find-class (env:client e4) e4 name)))))
        (load-source-file "CLOS/class-of-defun.lisp" e4))
      (enable-object-creation-in-e4 e4 e5)
      (create-additional-generic-functions e5)
      (setf (env:fdefinition (env:client e5) e5 'compile)
            (lambda (x lambda-expression)
              (assert (null x))
              (assert (and (consp lambda-expression) (eq (first lambda-expression) 'lambda)))
              (let* ((cst (cst:cst-from-expression lambda-expression))
                     (ast (cleavir-cst-to-ast:cst-to-ast (env:client e5) cst e5)))
                (ast-eval ast (env:client e5) e5))))
      (enable-typep e5)
      (enable-array-access e5)
      (load-source-file "CLOS/class-of-defun.lisp" e5)
      (enable-slot-value e5)
      (enable-object-creation e5)
      (enable-method-combinations e3 e5)
      (enable-generic-function-creation e5)
      (enable-printing e5)
      (finalize-inheritance e4))))
