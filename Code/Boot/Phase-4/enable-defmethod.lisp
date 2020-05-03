(cl:in-package #:sicl-boot-phase-4)

(defun define-make-specializer (e3)
  (setf (sicl-genv:fdefinition 'sicl-clos::make-specializer e3)
        (lambda (specializer)
          (cond ((symbolp specializer)
                 (sicl-genv:find-class specializer e3))
                (t
                 specializer)))))

(defun define-create-method-lambda (e4)
  (setf (sicl-genv:fdefinition 'sicl-clos::create-method-lambda e4)
        (lambda (function lambda-expression environment)
          (sicl-clos::make-method-lambda-default
           function nil lambda-expression environment))))

(defun enable-defmethod (boot)
  (with-accessors ((e3 sicl-boot:e3)
                   (e4 sicl-boot:e4))
      boot
    (setf (sicl-genv:fdefinition 'sicl-clos:make-method-lambda e4)
          #'sicl-clos::make-method-lambda-default)
    (define-make-specializer e3)
    (load-fasl "CLOS/make-method-for-generic-function.fasl" e3)
    (setf (sicl-genv:fdefinition 'sicl-clos::function-of-method e4)
          (sicl-genv:fdefinition 'sicl-clos::method-function e3))
    (sicl-boot:with-straddled-function-definitions
        ((sicl-clos::ensure-method) e4)
      (load-fasl "CLOS/ensure-method.fasl" e3))
    (define-create-method-lambda e4)))
