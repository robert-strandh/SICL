(cl:in-package #:sicl-boot-phase-6)

(defun import-functions-from-host-into-e5
    (names e3 e5)
  (loop with e3-client = (env:client e3)
        with make-instance = (env:fdefinition e3-client e3 'make-instance)
        with e5-client = (env:client e5)
        for name in names
        for host-function = (fdefinition name)
        for sicl-function = (funcall make-instance 'sicl-clos:simple-function)
        do (setf (sicl-boot:original-function sicl-function) host-function)
           (sicl-host-mop:set-funcallable-instance-function
            sicl-function host-function)
           (setf (env:fdefinition e5-client e5 name) sicl-function)))

(defun enable-generic-function-initialization (e3 e5)
  (load-source-file "CLOS/no-applicable-method-defgenerics.lisp" e5)
  (load-source-file "CLOS/no-applicable-method-defmethods.lisp" e5)
  (with-intercepted-function-cells
      (e5
       ;; FIXME: this one should also assign the slots in the
       ;; function object.
       (sicl-clos:set-funcallable-instance-function
        (list #'sicl-host-mop:set-funcallable-instance-function)))
    (load-source-file "CLOS/invalidate-discriminating-function.lisp" e5))
  (import-functions-from-host-into-e5
   '(cleavir-code-utilities:parse-generic-function-lambda-list
     cleavir-code-utilities:required)
   e3 e5)
  (load-source-file "CLOS/generic-function-initialization-support.lisp" e5)
  (load-source-file "CLOS/generic-function-initialization-defmethods.lisp" e5)
  (load-source-file "CLOS/ensure-generic-function-using-class-defgenerics.lisp" e5)
  (load-source-file "CLOS/ensure-generic-function-using-class-support.lisp" e5)
  (load-source-file "CLOS/ensure-generic-function-using-class-defmethods.lisp" e5))

(defun enable-generic-function-creation (e3 e5)
  (enable-generic-function-initialization e3 e5))
