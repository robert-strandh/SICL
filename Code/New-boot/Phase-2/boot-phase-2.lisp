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

(defmethod sicl-genv:typep
    (object (type-specifier (eql 'class)) (environment environment))
  t)

(defmethod sicl-genv:typep
    (object (type-specifier (eql 'generic-function)) (environment environment))
  t)

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
;;; method, ENSURE-METHOD calls GENERIC-FUNCTION-METHOD-CLASS which
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

;;; When we load the file containing ENSURE-METHOD, we also define the
;;; function MAKE-SPECIALIZER.  However that function does the wrong
;;; thing in case the specializer given is the symbol T, because then
;;; it looks up T in E1 and we get an automatic specialization on the
;;; host class FUNCALLABLE-STANDARD-OBJECT which is obviously not what
;;; we want.  So we define a special version of MAKE-SPECIALIZER here.
;;; It does not have to do any error checking, and it handles T
;;; specially by looking up the class T in the host environment
;;; instead of in E1.
(defun define-make-specializer (e1 e2)
  (setf (sicl-genv:fdefinition 'sicl-clos::make-specializer e2)
        (lambda (specializer environment)
          (declare (ignore environment))
          (cond ((eq specializer 't)
                 (find-class 't))
                ((symbolp specializer)
                 (sicl-genv:find-class specializer e1))
                (t
                 specializer)))))

;;; This function defines functions ADD-READER-METHOD and
;;; ADD-WRITER-METHOD in E2 that look up their generic functions in
;;; E3.  They do not call ADD-METHOD, because ADD-METHOD is defined to
;;; add methods to generic functions in E2.  The other defect these
;;; functions have is that they do not set the specializer profile on
;;; the generic function, so we have to do that manually in phase 3.
(defun define-add-accessor-method (e1 e2 e3)
  (setf (sicl-genv:fdefinition 'sicl-clos::add-reader-method e2)
        (lambda (class function-name slot-definition)
          (let* ((lambda-list '(object))
                 (generic-function (sicl-genv:fdefinition function-name e3))
                 (specializers (list class))
                 (slot-name (slot-value slot-definition 'sicl-clos::%name))
                 (method-function
                   (compile nil `(lambda (arguments next-methods)
                                   (declare (ignore next-methods))
                                   (slot-value (car arguments) ',slot-name))))
                 (method-class (sicl-genv:find-class
                                'sicl-clos:standard-reader-method e1))
                 (method (make-instance method-class
                           :lambda-list lambda-list
                           :qualifiers '()
                           :specializers specializers
                           :function method-function
                           :slot-definition slot-definition)))
            (push method (slot-value generic-function 'sicl-clos::%methods)))))
  (setf (sicl-genv:fdefinition 'sicl-clos::add-writer-method e2)
        (lambda (class function-name slot-definition)
          (let* ((lambda-list '(object))
                 (generic-function (sicl-genv:fdefinition function-name e3))
                 (specializers (list (sicl-genv:find-class 't e2) class))
                 (slot-name (slot-value slot-definition 'sicl-clos::%name))
                 (method-function
                   (compile nil `(lambda (arguments next-methods)
                                   (declare (ignore next-methods))
                                   (setf (slot-value (cadr arguments) ',slot-name)
                                         (car arguments)))))
                 (method-class (sicl-genv:find-class
                                'sicl-clos:standard-writer-method e1))
                 (method (make-instance method-class
                           :lambda-list lambda-list
                           :qualifiers '()
                           :specializers specializers
                           :function method-function
                           :slot-definition slot-definition)))
            (push method (slot-value generic-function 'sicl-clos::%methods))))))

;;; We already know how to execute a DEFGENERIC form in E2.  Now we
;;; need to know how to use DEFMETHOD to define methods on the generic
;;; functions we create with DEFGENERIC.  That is the purpose of this
;;; function.
(defun make-defmethod-possible-in-e2 (e1 e2)
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
   'add-method e2)
  (sicl-minimal-extrinsic-environment:import-function-from-host
   'copy-list e2)
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
  (load-file "CLOS/make-method-for-generic-function.lisp" e1)
  ;; FIXME: this one should move to a different phase.
  (load-file "CLOS/make-method-for-generic-function.lisp" e2)
  (import-functions-from-host
   '(cleavir-code-utilities:proper-list-p
     add-method)
   e1)
  (load-file "CLOS/make-specializer.lisp" e1)
  (load-file "CLOS/ensure-method.lisp" e2)
  (load-file "CLOS/defmethod-defmacro.lisp" e2))

;;; The problem we are solving here is that when we do
;;; (CALL-NEXT-METHOD) in the :AROUND method of SHARED-INITIALIZE, we
;;; attempt to take the METHOD-FUNCTION of the next method.  But
;;; METHOD-FUNCTION is a SICL function in E2 and it doesn't have any
;;; method for the host class STANDARD-METHOD.  We solve this problem
;;; by adding such a method.
(defun define-method-on-method-function (e2)
  (let ((temp (gensym)))
    (setf (fdefinition temp)
          (sicl-genv:fdefinition 'sicl-clos:method-function e2))
    (eval `(defmethod ,temp ((generic-function standard-method))
             (closer-mop:method-function generic-function)))
    (fmakunbound temp)))

(defun add-support-for-class-initialization-to-e2 (e1 e2 e3)
  (setf (sicl-genv:fdefinition 'sicl-clos:validate-superclass e2)
        (constantly t))
  (setf (sicl-genv:fdefinition 'sicl-clos:direct-slot-definition-class e2)
        (lambda (&rest arguments)
          (declare (ignore arguments))
          (sicl-genv:find-class 'sicl-clos:standard-direct-slot-definition e1)))
  (load-file "CLOS/add-remove-direct-subclass-support.lisp" e2)
  (load-file "CLOS/add-remove-direct-subclass-defgenerics.lisp" e2)
  (load-file "CLOS/add-remove-direct-subclass-defmethods.lisp" e2)
  (setf (sicl-genv:fdefinition 'sicl-clos:reader-method-class e2)
        (lambda (&rest arguments)
          (declare (ignore arguments))
          (sicl-genv:find-class 'sicl-clos:standard-reader-method e1)))
  (setf (sicl-genv:fdefinition 'sicl-clos:writer-method-class e2)
        (lambda (&rest arguments)
          (declare (ignore arguments))
          (find-class 'closer-mop:standard-writer-method)))
  (setf (sicl-genv:fdefinition 'compile e2)
        (lambda (name &optional definition)
          (assert (null name))
          (assert (not (null definition)))
          (cleavir-env:eval definition e2 e2)))
  (load-file "CLOS/add-accessor-method.lisp" e2)
  (setf (sicl-genv:fdefinition 'sicl-clos:default-superclasses e2)
        (lambda (class) (declare (ignore class)) '()))
  (load-file "CLOS/class-initialization-support.lisp" e2)
  (sicl-minimal-extrinsic-environment:import-function-from-host
   'shared-initialize e2)
  (load-file "CLOS/class-initialization-defmethods.lisp" e2)
  (define-method-on-method-function e2)
  (define-add-accessor-method e1 e2 e3))

(defun boot-phase-2 (boot)
  (format *trace-output* "Start of phase 2~%")
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)
                   (e4 sicl-new-boot:e4)) boot
    (change-class e2 'environment)
    (import-functions-from-host '(format) e1)
    (import-functions-from-host '(format) e2)
    (import-functions-from-host '(format) e3)
    (import-functions-from-host '(format) e4)
    (setf (sicl-genv:special-variable '*trace-output* e1 t) *trace-output*)
    (setf (sicl-genv:special-variable '*trace-output* e2 t) *trace-output*)
    (setf (sicl-genv:special-variable '*trace-output* e3 t) *trace-output*)
    (setf (sicl-genv:special-variable '*trace-output* e4 t) *trace-output*)
    (sicl-minimal-extrinsic-environment:import-package-from-host 'sicl-clos e3)
    (sicl-minimal-extrinsic-environment:import-function-from-host
     'sicl-clos:defclass-expander e2)
    (sicl-minimal-extrinsic-environment:import-function-from-host
     '(setf sicl-genv:special-variable) e2)
    ;; REMOVE is needed by the class initialization protocol.
    (sicl-minimal-extrinsic-environment:import-function-from-host
     'remove e2)
    (load-accessor-defgenerics boot)
    (define-make-instance e1 e2)
    (make-defmethod-possible-in-e2 e1 e2)
    (add-support-for-class-initialization-to-e2 e1 e2 e3)
    (create-mop-classes boot)))
