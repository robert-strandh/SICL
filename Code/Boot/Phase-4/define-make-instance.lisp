(cl:in-package #:sicl-boot-phase-4)

(defun define-make-instance (e2)
  ;; We define MAKE-INSTANCE in environment E2 so that it calls the
  ;; host MAKE-INSTANCE always with a class metaobject and never
  ;; with a symbol.  If our version receives a symbol, it looks up
  ;; the class metaobject in environment E2 before calling the host
  ;; version.
  (let ((client (env:client e2)))
    (setf (env:fdefinition client e2 'make-instance)
          (lambda (class-or-name &rest args)
            (let* ((class (if (symbolp class-or-name)
                              (env:find-class client e2 class-or-name)
                              class-or-name))
                   (result (apply #'make-instance class args)))
              result)))))
