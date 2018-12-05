(cl:in-package #:sicl-boot-phase-5)

(defun define-find-specializer-class-t-in-e5 (e5)
  (setf (sicl-genv:fdefinition 'sicl-clos::find-specializer-class-t e5)
        (lambda () (sicl-genv:find-class 't e5))))

;;; The specializers of the generic functions in E5 are the classes of
;;; the instances in E5, so they are the classes in E4.
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

(defun enable-defmethod-in-e5 (boot)
  (with-accessors ((e4 sicl-boot:e4)
                   (e5 sicl-boot:e5)) boot
    (define-find-specializer-class-t-in-e5 e5)
    (setf (sicl-genv:fdefinition 'sicl-clos:make-method-lambda e5)
          #'sicl-clos::make-method-lambda-default)
    (define-make-specializer e4)
    (load-file "CLOS/make-method-for-generic-function.lisp" e4)
    (setf (sicl-genv:fdefinition 'sicl-clos::add-method-to-generic-function e4)
          (sicl-genv:fdefinition 'sicl-clos::add-method e4))
    (import-functions-from-host '(copy-list) e5)
    (setf (sicl-genv:fdefinition 'sicl-clos::function-of-method e5)
          (sicl-genv:fdefinition 'sicl-clos::method-function e4))
    (load-file "CLOS/ensure-method.lisp" e4)
    (setf (sicl-genv:fdefinition 'sicl-clos::ensure-method e5)
          (sicl-genv:fdefinition 'sicl-clos::ensure-method e4))
    (define-create-method-lambda e5)
    (import-functions-from-host
     '(mapcar subseq 1+ elt position-if
       sicl-genv:fdefinition sicl-genv:fboundp
       cleavir-code-utilities:parse-specialized-lambda-list
       cleavir-code-utilities:separate-function-body
       cleavir-code-utilities:required)
     e5)
    (setf (sicl-genv:fdefinition 'sicl-clos::ensure-method-on-generic-function e5)
          (sicl-genv:fdefinition 'sicl-clos::ensure-method e5))
    (load-file "CLOS/defmethod-support.lisp" e5)
    (load-file "CLOS/defmethod-defmacro.lisp" e5)))
