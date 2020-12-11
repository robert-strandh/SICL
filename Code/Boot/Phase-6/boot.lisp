(cl:in-package #:sicl-boot-phase-6)

(defun boot (boot)
  (format *trace-output* "Start phase 6~%")
  (with-accessors ((e3 sicl-boot:e3)
                   (e4 sicl-boot:e4)
                   (e5 sicl-boot:e5))
      boot
    (load-source-file "CLOS/standard-instance-access.lisp" e4)
    (define-error-functions
        '(sicl-clos::all-descendants sicl-clos::cartesian-product)
        e4)
    (load-source-file "CLOS/satiation.lisp" e4)
    (satiate-generic-functions-1 e3 e4 e5)))
