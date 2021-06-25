(cl:in-package #:sicl-boot-phase-5)

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

(defun boot (boot)
  (format *trace-output* "Start phase 5~%")
  (with-accessors ((e0 sicl-boot:e0)
                   (e2 sicl-boot:e2)
                   (e3 sicl-boot:e3)
                   (e4 sicl-boot:e4)
                   (e5 sicl-boot:e5))
      boot
    (change-class e5 'environment
                  :client (make-instance 'client :environment e5))
    (load-source-file "Package-and-symbol/symbol-value-etc-defuns.lisp" e5)
    (sicl-boot:create-accessor-defgenerics e5)
    (sicl-boot:create-mop-classes e5)
    (create-additional-generic-functions e5)
    (load-source-file "Arithmetic/floor-defun.lisp" e5)
    (load-source-file "Arithmetic/ceiling-defun.lisp" e5)
    (load-source-file "Arithmetic/truncate-defun.lisp" e5)
    (load-source-file "Arithmetic/round-defun.lisp" e5)
    (prepare-next-phase e3 e4 e5)))
