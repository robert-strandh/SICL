(cl:in-package #:sicl-boot-phase-4)

(defun boot (boot)
  (format *trace-output* "Start phase 4~%")
  (with-accessors ((e2 sicl-boot:e2)
                   (e3 sicl-boot:e3)
                   (e4 sicl-boot:e4)
                   (e5 sicl-boot:e5))
      boot
    (change-class e4 'environment
                  :client (make-instance 'client :e4 e4))
    (load-source-file "Package-and-symbol/symbol-value-etc-defuns.lisp" e4)
    (sicl-boot:create-accessor-defgenerics e4)
    (sicl-boot:create-mop-classes e4)
    (load-source-file "Package-and-symbol/symbol-value-etc-defuns.lisp" e4)
    ;; (load-source-file "CLOS/class-readers-forward-referenced-class-defmethods.lisp" e4)
    ;; (load-source-file "CLOS/class-readers-defmethods-before.lisp" e4)
    (prepare-next-phase e3 e4 e5)))
