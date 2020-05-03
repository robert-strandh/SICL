(cl:in-package #:sicl-boot-phase-5)

(defun define-make-specializer (e4)
  (setf (sicl-genv:fdefinition 'sicl-clos::make-specializer e4)
        (lambda (specializer)
          (cond ((symbolp specializer)
                 (sicl-genv:find-class specializer e4))
                (t
                 specializer)))))

(defun define-create-method-lambda (e5)
  (setf (sicl-genv:fdefinition 'sicl-clos::create-method-lambda e5)
        (lambda (function lambda-expression environment)
          (sicl-clos::make-method-lambda-default
           function nil lambda-expression environment))))

(defun enable-defmethod (boot)
  (with-accessors ((e4 sicl-boot:e4)
                   (e5 sicl-boot:e5))
      boot
    (setf (sicl-genv:fdefinition 'sicl-clos:make-method-lambda e5)
          #'sicl-clos::make-method-lambda-default)
    (define-make-specializer e4)
    (load-fasl "CLOS/make-method-for-generic-function.fasl" e4)
    (setf (sicl-genv:fdefinition 'sicl-clos::function-of-method e5)
          (sicl-genv:fdefinition 'sicl-clos::method-function e4))
    (sicl-boot:with-straddled-function-definitions
        ((sicl-clos::ensure-method) e5)
      (load-fasl "CLOS/ensure-method.fasl" e4))
    (define-create-method-lambda e5)))
