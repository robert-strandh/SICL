(cl:in-package #:sicl-boot-phase-5)

(defun boot (boot)
  (format *trace-output* "Start phase 5~%")
  (with-accessors ((e0 sicl-boot:e0)
                   (e2 sicl-boot:e2)
                   (e3 sicl-boot:e3)
                   (e4 sicl-boot:e4)
                   (e5 sicl-boot:e5))
      boot
    (change-class e5 'environment)
    (change-class (env:client e5) 'client
                  ;; Until we create a cyclic graph, we must
                  ;; use the accessor from E4.
                  :static-environment-function
                  (env:fdefinition
                   (env:client e4) e4 'sicl-clos:environment))
    (prepare-this-phase e3 e4 e5)
    (load-source-file "Symbol/symbol-value-etc-defuns.lisp" e5)
    (sicl-boot:create-accessor-defgenerics e5)
    (sicl-boot:create-mop-classes e5)
    (create-additional-generic-functions e5)
    (load-source-file "Arithmetic/floor-defun.lisp" e5)
    (load-source-file "Arithmetic/ceiling-defun.lisp" e5)
    (load-source-file "Arithmetic/truncate-defun.lisp" e5)
    (load-source-file "Arithmetic/round-defun.lisp" e5)))
