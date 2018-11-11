(cl:in-package #:sicl-new-boot-phase-3)

(defun define-find-specializer-class-t-in-e3 (e3)
  (setf (sicl-genv:fdefinition 'sicl-clos::find-specializer-class-t e3)
        (lambda () (sicl-genv:find-class 't e3))))

;;; The specializers of the generic functions in E3 are the classes of
;;; the instances in E3, so they are the classes in E2.
(defun define-make-specializer (e2)
  (setf (sicl-genv:fdefinition 'sicl-clos::make-specializer e2)
        (lambda (specializer)
          (cond ((symbolp specializer)
                 (sicl-genv:find-class specializer e2))
                (t
                 specializer)))))

(defun define-create-method-lambda (e3)
  (setf (sicl-genv:fdefinition 'sicl-clos::create-method-lambda e3)
        (lambda (function lambda-expression environment)
          (sicl-clos::make-method-lambda-default
           function nil lambda-expression environment))))

(defun enable-defmethod-in-e3 (boot)
  (with-accessors ((e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)) boot
    (define-find-specializer-class-t-in-e3 e3)
    (setf (sicl-genv:fdefinition 'sicl-clos:method-function e3)
          (sicl-genv:fdefinition 'sicl-clos:method-function e2))
    (setf (sicl-genv:fdefinition 'sicl-clos:make-method-lambda e3)
          #'sicl-clos::make-method-lambda-default)
    (define-make-specializer e2)
    (load-file "CLOS/make-method-for-generic-function.lisp" e2)
    (setf (sicl-genv:fdefinition 'sicl-clos::add-method-to-generic-function e2)
          (sicl-genv:fdefinition 'sicl-clos::add-method e2))
    (import-functions-from-host '(copy-list) e3)
    (setf (sicl-genv:fdefinition 'sicl-clos::function-of-method e3)
          (sicl-genv:fdefinition 'sicl-clos::method-function e3))
    (load-file "CLOS/ensure-method.lisp" e2)
    (setf (sicl-genv:fdefinition 'sicl-clos::ensure-method e3)
          (sicl-genv:fdefinition 'sicl-clos::ensure-method e2))
    (define-create-method-lambda e3)
    (import-functions-from-host
     '(mapcar subseq 1+ elt position-if
       sicl-genv:fdefinition sicl-genv:fboundp
       cleavir-code-utilities:parse-specialized-lambda-list
       cleavir-code-utilities:separate-function-body
       cleavir-code-utilities:required)
     e3)
    (setf (sicl-genv:fdefinition 'sicl-clos::ensure-method-on-generic-function e3)
          (sicl-genv:fdefinition 'sicl-clos::ensure-method e3))
    (load-file "CLOS/defmethod-support.lisp" e3)
    (load-file "CLOS/defmethod-defmacro.lisp" e3)))
