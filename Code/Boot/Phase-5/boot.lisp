(cl:in-package #:sicl-boot-phase-5)

(defun boot (boot)
  (format *trace-output* "Start phase 5~%")
  (with-accessors ((e0 sicl-boot:e0)
                   (e3 sicl-boot:e3)
                   (e4 sicl-boot:e4)
                   (e5 sicl-boot:e5))
      boot
    (change-class e5 'environment :client (make-instance 'client))
    (sicl-boot:create-accessor-defgenerics e5)
    (sicl-boot:create-mop-classes e5)
    (with-intercepted-function-cells
        (e4
         (find-class
          (list (lambda (name)
                  (env:find-class (env:client e4) e4 name)))))
      (load-source-file "CLOS/class-of-defun.lisp" e4))
    (enable-object-creation e4 e5)
    (create-additional-generic-functions e5)))
