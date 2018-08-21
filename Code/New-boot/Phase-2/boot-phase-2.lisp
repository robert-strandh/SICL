(cl:in-package #:sicl-new-boot-phase-2)

;;; We define MAKE-INSTANCE in environment E2 so that it calls the
;;; host MAKE-INSTANCE always with a class metaobject and never with a
;;; symbol.  If our version receives a symbol, it looks up the class
;;; metaobject in environment E1 before calling the host version.
(defun define-make-instance (e1 e2)
  (setf (sicl-genv:fdefinition 'make-instance e2)
        (lambda (class &rest arguments)
          (apply #'make-instance
                 (if (symbolp class)
                     (sicl-genv:find-class class e1)
                     class)
                 arguments))))

;;; When we need to find a class in E2, like for creating a method
;;; metaobject, or for finding a specializer for some method, we need
;;; to find a host class, and the host classes are present in E1, so
;;; we need a special version of SICL-GENV:FIND-CLASS in E2 that
;;; ignores its ENVIRONMENT parameter and looks up the class in E1
;;; instead.
(defun define-find-class (e1 e2)
  (setf (sicl-genv:fdefinition 'sicl-genv:find-class e2)
        (lambda (class-name environment)
          (declare (ignore environment))
          (sicl-genv:find-class class-name e1))))

(defclass environment (sicl-minimal-extrinsic-environment:environment)
  ())

(defmethod sicl-genv:class-of (object (environment environment))
  (class-of object))

;;; The problem that we are solving with this function is the
;;; following: In phase 1, we loaded a bunch of definitions of host
;;; generic functions into E2.  We also loaded definitions of host
;;; classes (corresponding to MOP classes) into E1.  When those
;;; classes were loaded, methods corresponding to class accessors were
;;; added to the generic functions in E2.  Among them,
;;; GENERIC-FUNCTION-METHOD-CLASS.  That method works when given an
;;; instance of the GENERIC-FUNCTION class defined in E1, but it
;;; doesn't work when given an instance of the host
;;; STANDARD-GENERIC-FUNCTION.  Now, in phase 2, we need to add
;;; methods to the generic functions in E2, and to create such a
;;; method, ensure-method calls GENERIC-FUNCTION-METHOD-CLASS which
;;; won't work.
;;;
;;; We solve the problem by adding a method on
;;; GENERIC-FUNCTION-METHOD-CLASS in E2 that calls the host version of
;;; the function with that name.
(defun define-method-on-generic-function-method-class (e2)
  (let ((temp (gensym)))
    (setf (fdefinition temp)
          (sicl-genv:fdefinition 'sicl-clos:generic-function-method-class e2))
    (eval `(defmethod ,temp ((generic-function standard-generic-function))
             (closer-mop:generic-function-method-class generic-function)))
    (fmakunbound temp)))

(defun boot-phase-2 (boot)
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)) boot
    (change-class e2 'environment)
    (sicl-minimal-extrinsic-environment:import-package-from-host 'sicl-clos e3)
    (sicl-minimal-extrinsic-environment:import-function-from-host
     'sicl-clos:defclass-expander e2)
    (sicl-minimal-extrinsic-environment:import-function-from-host
     '(setf sicl-genv:special-variable) e2)
    ;; REMOVE is needed by the class initialization protocol.
    (sicl-minimal-extrinsic-environment:import-function-from-host
     'remove e2)
    (load-accessor-defgenerics boot)
    (create-mop-classes boot)
    (define-make-instance e1 e2)
    ;; FIND-CLASS is used by ENSURE-METHOD to look up a class as a
    ;; specializer when a symbol is given.
    (define-find-class e1 e2)
    ;; TYPEP is used by ENSURE-METHOD to check that, if a symbol was
    ;; not given, then an instance of SPECIALIZER was.
    (sicl-minimal-extrinsic-environment:import-function-from-host
     'sicl-genv:typep e2)
    ;; PROPER-LIST-P is used by ENSURE-METHOD to check that the list
    ;; of specializers given is a proper list.
    (sicl-minimal-extrinsic-environment:import-function-from-host
     'cleavir-code-utilities:proper-list-p e2)
    (define-method-on-generic-function-method-class e2)
    (sicl-minimal-extrinsic-environment:import-function-from-host
     'add-method e2)))
