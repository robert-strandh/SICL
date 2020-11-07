(cl:in-package #:sicl-boot-phase-6)

(defun boot (boot)
  (format *trace-output* "Start phase 6~%")
  (with-accessors ((e4 sicl-boot:e4)
                   (e5 sicl-boot:e5))
      boot
    (copy-classes e4 e5)
    (load-source-file "CLOS/class-of-defun.lisp" e5)
    (enable-slot-value e5)
    (enable-object-creation e5)
    (enable-method-combinations e5)
    (enable-generic-function-creation e5)))
