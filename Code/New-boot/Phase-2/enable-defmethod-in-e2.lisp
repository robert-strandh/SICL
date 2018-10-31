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

(defun define-make-specializer-in-e1 (boot)
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)) boot
    (setf (sicl-genv:fdefinition 'sicl-clos::make-specializer e1)
          (lambda (specializer)
            (cond ((eq specializer 't)
                   (find-class 't))
                  ((symbolp specializer)
                   (sicl-genv:find-class specializer e1))
                  ((sicl-genv:typep specializer 'specializer e1)
                   specializer)
                  (t
                   (error "Specializer must be symbol or specializer metaobject: ~s"
                          specializer)))))))

(defun enable-defmethod-in-e2 (boot)
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)) boot
    ;; FIND-CLASS is used by ENSURE-METHOD to look up a class as a
    ;; specializer when a symbol is given.
    (define-find-class-in-e2 e1 e2)
    (setf (sicl-genv:fdefinition 'sicl-clos:make-method-lambda e1)
          #'sicl-clos::make-method-lambda-default)
    (setf (sicl-genv:fdefinition 'ensure-generic-function e1)
          (sicl-genv:fdefinition 'ensure-generic-function e2))
    (import-functions-from-host
     '(mapcar
       subseq
       elt
       position-if
       sicl-genv:find-class
       ;; TYPEP is used by ENSURE-METHOD to check that, if a symbol
       ;; was not given, then an instance of SPECIALIZER was.
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
    ;; 1+ is called by PARSE-METHOD to obtain an interval designator
    ;; for SUBSEQ in order to parse the method body.
    (import-function-from-host '1+ e1)
    (load-file "CLOS/defmethod-support.lisp" e1)
    (setf (sicl-genv:fdefinition 'sicl-clos:defmethod-expander e2)
          (sicl-genv:fdefinition 'sicl-clos:defmethod-expander e1))
    (import-functions-from-host
     '(add-method
       copy-list)
     e1)
    (load-file "CLOS/make-specializer.lisp" e1)
    (load-file "CLOS/make-method-for-generic-function.lisp" e1)
    (load-file "CLOS/ensure-method.lisp" e1)
    (setf (sicl-genv:fdefinition 'sicl-clos:ensure-method e2)
          (sicl-genv:fdefinition 'sicl-clos:ensure-method e1))
    (define-make-specializer-in-e1 boot)
    (load-file "CLOS/defmethod-defmacro.lisp" e2)))
