(cl:in-package #:sicl-boot-phase-4)

(defun finalize-classes (e3 e4)
  (format *trace-output* "Finalizing all classes in ~a..." (sicl-boot:name e4))
  (finish-output *trace-output*)
  (let ((visited (make-hash-table :test #'eq))
        (finalized-p (env:fdefinition (env:client e3) e3 'sicl-clos::class-finalized-p))
        (finalize (env:fdefinition (env:client e3) e3 'sicl-clos:finalize-inheritance)))
    (do-all-symbols (symbol)
      (unless (gethash symbol visited)
        (setf (gethash symbol visited) t)
        (let ((class (env:find-class (env:client e4) e4 symbol)))
          (unless (or (null class) (funcall finalized-p class))
            (funcall finalize class))))))
  (format *trace-output* "done~%")
  (finish-output *trace-output*))

(defun enable-object-allocation (e4)
  (setf (env:fdefinition (env:client e4) e4 'sicl-clos::allocate-general-instance)
        (lambda (class size)
          (make-instance 'sicl-boot:header
            :class class
            :rack (make-array size :initial-element 10000000))))
  (load-source-file "CLOS/stamp-offset-defconstant.lisp" e4)
  (load-source-file "CLOS/effective-slot-definition-class-support.lisp" e4)
  (load-source-file "CLOS/effective-slot-definition-class-defgeneric.lisp" e4)
  (load-source-file "CLOS/effective-slot-definition-class-defmethods.lisp" e4)
  ;; These were already loaded in phase 3 because they were needed for
  ;; the finalization of built-in classes.
  ;; (load-source-file "CLOS/class-finalization-defgenerics.lisp" e4)
  ;; (load-source-file "CLOS/class-finalization-defmethods.lisp" e4)
  (load-source-file "CLOS/allocate-instance-support.lisp" e4)
  (load-source-file "CLOS/allocate-instance-defgenerics.lisp" e4)
  (load-source-file "CLOS/allocate-instance-defmethods.lisp" e4))

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

(defun define-ast-eval (e5)
  (setf (env:fdefinition (env:client e5) e5 'sicl-boot:ast-eval)
        (lambda (ast)
          (let* ((client (env:client e5))
                 (hir (sicl-ast-to-hir:ast-to-hir client ast))
                 (fun (sicl-hir-evaluator:top-level-hir-to-host-function client hir))
                 (sicl-run-time:*dynamic-environment* '())
                 (function-cell-function
                   (env:fdefinition
                    client e5 'sicl-data-and-control-flow:function-cell)))
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
                                    (sicl-hir-transformations:constants hir))))))))

(defun prepare-next-phase (e3 e4 e5)
  (define-ast-eval e5)
  (sicl-boot:copy-macro-functions e4 e5)
  (load-source-file "CLOS/class-of-defun.lisp" e4)
  (enable-typep e3 e4)
  (enable-object-creation e3 e4)
  (enable-method-combinations e3 e4 e5)
  (setf (env:special-operator (env:client e5) e5 'cleavir-primop:multiple-value-call) t)
  (setf (env:fdefinition (env:client e4) e4 'compile)
        (lambda (x lambda-expression)
          (assert (null x))
          (assert (and (consp lambda-expression) (eq (first lambda-expression) 'lambda)))
          (let* ((cst (cst:cst-from-expression lambda-expression))
                 (ast (cleavir-cst-to-ast:cst-to-ast (env:client e4) cst e4)))
            (with-intercepted-function-cells
                (e4
                 (make-instance
                  (env:function-cell (env:client e3) e3 'make-instance))
                 (sicl-clos:method-function
                  (env:function-cell (env:client e3) e3 'sicl-clos:method-function)))
              (funcall (env:fdefinition (env:client e4) e4 'sicl-boot:ast-eval)
                       ast)))))
  (enable-compute-discriminating-function e3 e4 e5)
  (load-source-file "CLOS/defgeneric-support.lisp" e5)
  ;; (import-functions-from-host
  ;;  '(cleavir-code-utilities:parse-generic-function-lambda-list
  ;;    cleavir-code-utilities:required)
  ;;  e4)
  (with-intercepted-function-cells
      (e4
       (sicl-clos:set-funcallable-instance-function
        (list #'closer-mop:set-funcallable-instance-function)))
    (load-source-file "CLOS/invalidate-discriminating-function.lisp" e4))
  ;; (import-functions-from-host
  ;;  '(cleavir-code-utilities:parse-generic-function-lambda-list
  ;;    cleavir-code-utilities:required)
  ;;  e4)
  (load-source-file "CLOS/generic-function-initialization-support.lisp" e4)
  (load-source-file "CLOS/generic-function-initialization-defmethods.lisp" e4)
  (enable-defgeneric e3 e4 e5)
  (enable-defmethod e3 e4 e5)
  (enable-defclass e3 e4 e5)
  (finalize-classes e3 e4))
