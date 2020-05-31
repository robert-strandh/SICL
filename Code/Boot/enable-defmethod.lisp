(cl:in-package #:sicl-boot)

(defun define-make-specializer (ea)
  (setf (sicl-genv:fdefinition 'sicl-clos::make-specializer ea)
        (lambda (specializer)
          (cond ((symbolp specializer)
                 (let ((result (sicl-genv:find-class specializer ea)))
                   (if (null result)
                       (error "There is no class named ~s." specializer)
                       result)))
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
  (load-source "CLOS/make-method-for-generic-function.lisp" ea)
  (setf (sicl-genv:fdefinition 'sicl-clos::function-of-method eb)
        (sicl-genv:fdefinition 'sicl-clos::method-function ea))
  (with-straddled-function-definitions
      ((sicl-clos::ensure-method) eb)
    (load-source "CLOS/ensure-method.lisp" ea))
  (define-create-method-lambda eb))
