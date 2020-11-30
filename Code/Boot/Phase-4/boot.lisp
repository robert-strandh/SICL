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
    ;; (setf (env:fdefinition (env:client e4) e4 'class-of)
    ;;       (lambda (object)
    ;;         (slot-value object 'sicl-boot::%class)))
    (sicl-boot:create-accessor-defgenerics e4)
    (sicl-boot:create-mop-classes e4)
    ;; (load-source-file "CLOS/class-readers-forward-referenced-class-defmethods.lisp" e4)
    ;; (load-source-file "CLOS/class-readers-defmethods-before.lisp" e4)
    (prepare-next-phase e2 e3 e4 e5)))
