(cl:in-package #:sicl-boot-phase-5)

(defun boot-phase-5 (boot)
  (with-accessors ((e3 sicl-boot:e3)
                   (e4 sicl-boot:e4)
                   (e5 sicl-boot:e5))
      boot
    (funcall (sicl-genv:fdefinition 'sicl-clos:compute-discriminating-function e4)
	     (sicl-genv:fdefinition 'class-name e5))))
