(cl:in-package #:sicl-boot)

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

(defun enable-defmethod (load-fasl ea eb)
  (setf (sicl-genv:fdefinition 'sicl-clos:make-method-lambda eb)
        #'sicl-clos::make-method-lambda-default)
  (define-make-specializer ea)
  (funcall load-fasl "CLOS/make-method-for-generic-function.fasl" ea)
  (setf (sicl-genv:fdefinition 'sicl-clos::function-of-method eb)
        (sicl-genv:fdefinition 'sicl-clos::method-function ea))
  (with-straddled-function-definitions
      ((sicl-clos::ensure-method) eb)
    (funcall load-fasl "CLOS/ensure-method.fasl" ea))
  (define-create-method-lambda eb))
