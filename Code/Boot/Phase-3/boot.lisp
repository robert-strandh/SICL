(cl:in-package #:sicl-boot-phase-3)
(defun boot (boot)
  (format *trace-output* "Start phase 3~%")
  (with-accessors ((e0 sicl-boot:e0)
                   (e2 sicl-boot:e2)
                   (e3 sicl-boot:e3)
                   (e4 sicl-boot:e4))
      boot
    (change-class e3 'environment :client (make-instance 'client))
    (sicl-boot:create-accessor-defgenerics e3)
    (sicl-boot:create-mop-classes e3)
    (load-source-file "CLOS/class-readers-forward-referenced-class-defmethods.lisp" e3)
    (load-source-file "CLOS/class-readers-defmethods-before.lisp" e3)
    (sicl-boot:copy-macro-functions e0 e4)
    (setf (sicl-boot:overridden-function-cells e4)
          `((find-class
             . (,(lambda (name &optional (errorp t) environment)
                 (declare (ignore environment))
                 (let ((result (env:find-class (env:client e3) e3 name)))
                   (if (and (null result) errorp)
                       (error "no class named ~s in E3" name)
                       result)))))
            ;; When we load a DEFMETHOD form into E4, the expansion of
            ;; that form contains a definition of CALL-NEXT-METHOD and
            ;; the that definition calls SICL-CLOS:METHOD-FUNCTION.
            ;; But we want the version of SICL-CLOS:METHOD-FUNCTION in
            ;; E3, so we need to override it here.
            (sicl-clos:method-function
             . ,(env:function-cell (env:client e3) e3 'sicl-clos:method-function))))
    (prepare-next-phase e2 e3 e4)))
