(cl:in-package #:sicl-boot-phase-5)

(defun boot (boot)
  (format *trace-output* "Start phase 5~%")
  (with-accessors ((e0 sicl-boot:e0)
                   (e2 sicl-boot:e2)
                   (e3 sicl-boot:e3)
                   (e4 sicl-boot:e4)
                   (e5 sicl-boot:e5))
      boot
    (change-class e5 'environment
                  :client (make-client 'client :environment e5))
    (load-source-file "Package-and-symbol/symbol-value-etc-defuns.lisp" e5)
    (sicl-boot:create-accessor-defgenerics e5)
    (sicl-boot:create-mop-classes e5)
    (create-additional-generic-functions e5)
    (prepare-next-phase e3 e4 e5)))
