(cl:in-package #:sicl-new-boot-phase-2)

;;; We already know how to execute a DEFGENERIC form in E2.  Now we
;;; need to know how to use DEFMETHOD to define methods on the generic
;;; functions we create with DEFGENERIC.  That is the purpose of this
;;; function.

;;; When we need to find a class in E2, like for creating a method
;;; metaobject, or for finding a specializer for some method, we need
;;; to find a host class, and the host classes are present in E1, so
;;; we need a special version of SICL-GENV:FIND-CLASS in E2 that
;;; ignores its ENVIRONMENT parameter and looks up the class in E1
;;; instead.
(defun define-find-class-in-e2 (e1 e2)
  (setf (sicl-genv:fdefinition 'sicl-genv:find-class e2)
        (lambda (class-name environment)
          (declare (ignore environment))
          (sicl-genv:find-class class-name e1))))

(defun enable-defmethod-in-e2 (boot)
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)) boot
    ;; FIND-CLASS is used by ENSURE-METHOD to look up a class as a
    ;; specializer when a symbol is given.
    (define-find-class-in-e2 e1 e2)
    ;; TYPEP is used by ENSURE-METHOD to check that, if a symbol was
    ;; not given, then an instance of SPECIALIZER was.
    (import-function-from-host 'sicl-genv:typep e2)
    ;; PROPER-LIST-P is used by ENSURE-METHOD to check that the list
    ;; of specializers given is a proper list.
    (import-function-from-host 'cleavir-code-utilities:proper-list-p e2)
    (define-method-on-generic-function-method-class e2)
    (import-function-from-host 'add-method e2)
    (import-function-from-host 'copy-list e2)
    (setf (sicl-genv:fdefinition 'sicl-clos:make-method-lambda e1)
          #'sicl-clos::make-method-lambda-default)
    (setf (sicl-genv:fdefinition 'ensure-generic-function e1)
          (sicl-genv:fdefinition 'ensure-generic-function e2))
    (import-functions-from-host
     '(mapcar
       subseq
       1+
       elt
       position-if
       sicl-genv:find-class
       sicl-genv:typep
       sicl-genv:fboundp
       sicl-genv:fdefinition
       cleavir-code-utilities:separate-function-body
       cleavir-code-utilities:required
       cleavir-code-utilities:parse-specialized-lambda-list)
     e1)
    (setf (sicl-genv:fdefinition 'sicl-clos:class-prototype e1)
          #'closer-mop:class-prototype)
    (setf (sicl-genv:fdefinition 'sicl-clos:generic-function-method-class e1)
          #'closer-mop:generic-function-method-class)
    (load-file "CLOS/defmethod-support.lisp" e1)
    (setf (sicl-genv:fdefinition 'sicl-clos:defmethod-expander e2)
          (sicl-genv:fdefinition 'sicl-clos:defmethod-expander e1))
    ;; ENSURE-METHOD calls MAKE-SPECIALIZER, but the version of
    ;; MAKE-SPECIALIZER that is in the file with that name is not
    ;; correct for phase 2.  Fix the problem by defining a special
    ;; version of it.
    (define-make-specializer e1 e2)
    (load-file "CLOS/make-method-for-generic-function.lisp" e2)
    (import-functions-from-host
     '(cleavir-code-utilities:proper-list-p
       add-method
       copy-list)
     e1)
    (load-file "CLOS/make-specializer.lisp" e1)
    (load-file "CLOS/ensure-method.lisp" e2)
    (load-file "CLOS/defmethod-defmacro.lisp" e2)))
