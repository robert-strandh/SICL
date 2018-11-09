(cl:in-package #:sicl-new-boot-phase-3)

;;; The specializers of the generic functions in E3 are the classes of
;;; the instances in E3, so they are the classes in E2.
(defun define-make-specializer (e2)
  (setf (sicl-genv:fdefinition 'sicl-clos::make-specializer e2)
        (lambda (specializer)
          (cond ((symbolp specializer)
                 (sicl-genv:find-class specializer e2))
                (t
                 specializer)))))

(defun define-defmethod-expander (boot)
  (with-accessors ((e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)) boot
    (setf (sicl-genv:fdefinition 'sicl-clos:defmethod-expander e3)
          (lambda (ct-env function-name rest)
            (multiple-value-bind
                  (qualifiers required remaining
                   specializers declarations documentation forms)
                (sicl-clos::parse-defmethod rest)
              (let* ((lambda-list (append required remaining))
                     (generic-function-var (gensym)))
                `(let* ((,generic-function-var
                          (ensure-generic-function ',function-name :environment ,ct-env)))
                   (sicl-clos:ensure-method
                    ,generic-function-var
                    :lambda-list ',lambda-list
                    :qualifiers ',qualifiers
                    :specializers ,(sicl-clos::canonicalize-specializers specializers)
                    :documentation ,documentation
                    :function
                    ,(funcall
                      (sicl-genv:fdefinition 'sicl-clos:make-method-lambda e3)
                      nil
                      nil
                      `(lambda ,lambda-list
                         ,@declarations
                         ,@forms)
                      nil)))))))))

(defun enable-defmethod-in-e3 (boot)
  (with-accessors ((e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)) boot
    (setf (sicl-genv:fdefinition 'sicl-clos:method-function e3)
          (sicl-genv:fdefinition 'sicl-clos:method-function e2))
    (setf (sicl-genv:fdefinition 'sicl-clos:make-method-lambda e3)
          #'sicl-clos::make-method-lambda-default)
    (define-make-specializer e2)
    (load-file "CLOS/make-method-for-generic-function.lisp" e2)
    (import-functions-from-host '(copy-list) e3)
    (setf (sicl-genv:fdefinition 'temp e2)
          (sicl-genv:fdefinition 'sicl-clos::ensure-method e2))
    (setf (sicl-genv:fdefinition 'sicl-clos::function-of-method e3)
          (sicl-genv:fdefinition 'sicl-clos::method-function e3))
    (load-file "CLOS/ensure-method.lisp" e2)
    (setf (sicl-genv:fdefinition 'sicl-clos::ensure-method e3)
          (sicl-genv:fdefinition 'sicl-clos::ensure-method e2))
    (setf (sicl-genv:fdefinition 'sicl-clos::ensure-method e2)
          (sicl-genv:fdefinition 'temp e2))
    (define-defmethod-expander boot)
    (load-file "CLOS/defmethod-defmacro.lisp" e3)))
