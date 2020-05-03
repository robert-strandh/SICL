(cl:in-package #:sicl-boot-phase-6)

(defun define-make-specializer (ea)
  (setf (sicl-genv:fdefinition 'sicl-clos::make-specializer ea)
        (lambda (specializer)
          (cond ((symbolp specializer)
                 (sicl-genv:find-class specializer ea))
                (t
                 specializer)))))

(defun define-create-method-lambda (eb)
  (setf (sicl-genv:fdefinition 'sicl-clos::create-method-lambda eb)
        (lambda (function lambda-expression environment)
          (sicl-clos::make-method-lambda-default
           function nil lambda-expression environment))))

(defun enable-defmethod (ea eb)
  (setf (sicl-genv:fdefinition 'sicl-clos:make-method-lambda eb)
        #'sicl-clos::make-method-lambda-default)
  (define-make-specializer ea)
  (load-fasl "CLOS/make-method-for-generic-function.fasl" ea)
  (setf (sicl-genv:fdefinition 'sicl-clos::function-of-method eb)
        (sicl-genv:fdefinition 'sicl-clos::method-function ea))
  (sicl-boot:with-straddled-function-definitions
      ((sicl-clos::ensure-method) eb)
    (load-fasl "CLOS/ensure-method.fasl" ea))
  (define-create-method-lambda eb))
