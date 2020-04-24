(cl:in-package #:sicl-boot-phase-6)

(defun define-find-specializer-class-t-in-e6 (e6)
  (setf (sicl-genv:fdefinition 'sicl-clos::find-specializer-class-t e6)
        (lambda () (sicl-genv:find-class 't e6))))

;;; The specializers of the generic functions in E6 are the classes of
;;; the instances in E6, so they are the classes in E5.
(defun define-make-specializer (e5)
  (setf (sicl-genv:fdefinition 'sicl-clos::make-specializer e5)
        (lambda (specializer)
          (cond ((symbolp specializer)
                 (sicl-genv:find-class specializer e5))
                (t
                 specializer)))))

(defun define-create-method-lambda (e6)
  (setf (sicl-genv:fdefinition 'sicl-clos::create-method-lambda e6)
        (lambda (function lambda-expression environment)
          (sicl-clos::make-method-lambda-default
           function nil lambda-expression environment))))

(defun enable-defmethod (boot)
  (with-accessors ((e5 sicl-boot:e5)
                   (e6 sicl-boot:e6))
      boot
    (define-find-specializer-class-t-in-e6 e6)
    (setf (sicl-genv:fdefinition 'sicl-clos:make-method-lambda e6)
          #'sicl-clos::make-method-lambda-default)
    (define-make-specializer e5)
    (load-fasl "CLOS/make-method-for-generic-function.fasl" e5)
    (setf (sicl-genv:fdefinition 'sicl-clos::add-method-to-generic-function e5)
          (sicl-genv:fdefinition 'sicl-clos::add-method e5))
    (import-functions-from-host '(copy-list) e6)
    (setf (sicl-genv:fdefinition 'sicl-clos::function-of-method e6)
          (sicl-genv:fdefinition 'sicl-clos::method-function e5))
    (sicl-boot:with-straddled-function-definitions
        ((sicl-clos::ensure-method) e5 e6)
      (load-fasl "CLOS/ensure-method.fasl" e5))
    (define-create-method-lambda e6)
    (import-functions-from-host
     '(mapcar subseq 1+ elt position-if
       sicl-genv:fdefinition sicl-genv:fboundp
       cleavir-code-utilities:parse-specialized-lambda-list
       cleavir-code-utilities:separate-function-body
       cleavir-code-utilities:required)
     e6)))
