(cl:in-package #:sicl-boot)

(defun define-make-instance (boot)
  (setf (sicl-genv:fdefinition 'make-instance (r2 boot))
        (lambda (class-name-or-class &rest arguments)
          (let ((class (if (symbolp class-name-or-class)
                           (sicl-genv:find-class class-name-or-class (r1 boot))
                           class-name-or-class)))
            (apply #'make-instance
                   class
                   arguments)))))
