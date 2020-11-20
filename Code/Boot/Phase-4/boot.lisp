(cl:in-package #:sicl-boot-phase-4)

(defun boot (boot)
  (format *trace-output* "Start phase 4~%")
  (with-accessors ((e0 sicl-boot:e0)
                   (e2 sicl-boot:e2)
                   (e3 sicl-boot:e3)
                   (e4 sicl-boot:e4)
                   (e5 sicl-boot:e5))
      boot
    (change-class e4 'environment
                  :client (make-instance 'client :e4 e4))
    ;; Define CLASS-OF temporarily here.  A production version will be
    ;; loaded once we have enough classes in E4 to be able to return
    ;; classes for FIXNUM, SINGLE-FLOAT, CHARACTER, and CONS.
    (setf (env:fdefinition (env:client e4) e4 'class-of)
          (lambda (object)
            (slot-value object 'sicl-boot::%class)))
    (sicl-boot:create-accessor-defgenerics e4)
    (sicl-boot:create-mop-classes e4)
    ;; I have no idea why this one is needed.
    (let ((fun (env:fdefinition (env:client e3) e3 'sicl-clos:method-function)))
      (setf (env:fdefinition (env:client e3) e3 'sicl-clos:method-function)
            (lambda (method)
              (if (eq (class-of method) (find-class 'standard-method))
                  (closer-mop:method-function method)
                  (funcall fun method)))))
    (load-source-file "CLOS/class-readers-forward-referenced-class-defmethods.lisp" e4)
    (load-source-file "CLOS/class-readers-defmethods-before.lisp" e4)
    (sicl-boot:copy-macro-functions e0 e5)
    (prepare-next-phase e2 e3 e4 e5)
    ;; We must remove the override on METHOD-FUNCTION in E4 before we
    ;; start loading things into E5, because when we start loading
    ;; things in E5, we will call COMPILE in E4 in order to create
    ;; effective method functions and discriminating functions, and
    ;; the forms to be compiled contain calls to METHOD-FUNCTION.  But
    ;; this time, we need for it to be the version of METHOD function
    ;; that is defined in E4.  There might be a better way of doing
    ;; this, but I haven't figured one out yet.
    (setf (sicl-boot:overridden-function-cells e4)
          (remove 'sicl-clos:method-function
                  (sicl-boot:overridden-function-cells e4)
                  :key #'car :test #'eq))))
