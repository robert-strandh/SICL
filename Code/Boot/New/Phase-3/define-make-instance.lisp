(cl:in-package #:sicl-boot-phase-3)

(defun define-make-instance (boot)
  (with-accessors ((e2 sicl-boot:e2) (e3 sicl-boot:e3)) boot
    ;; We define MAKE-INSTANCE in environment E2 so that it calls the
    ;; host MAKE-INSTANCE always with a class metaobject and never
    ;; with a symbol.  If our version receives a symbol, it looks up
    ;; the class metaobject in environment E2 before calling the host
    ;; version.
    (setf (sicl-genv:fdefinition 'make-instance e2)
          (lambda (class-or-name &rest args)
            (let* ((class (if (symbolp class-or-name)
                              (sicl-genv:find-class class-or-name e2)
                              class-or-name))
                   (result (apply #'make-instance class args)))
              result)))
    ;; MAKE-INSTANCE is called in environment E3 when DEFMETHOD is
    ;; called in environment E3 to create a method to add to a bridge
    ;; generic function in E3.
    (setf (sicl-genv:fdefinition 'make-instance e3)
          (lambda (class-or-name &rest args)
            (let* ((class (if (symbolp class-or-name)
                              (sicl-genv:find-class class-or-name e2)
                              class-or-name))
                   (result (apply #'make-instance class args)))
              result)))))
