(cl:in-package #:sicl-new-boot-phase-2)

;;; We define MAKE-INSTANCE in environment E1 so that it calls the
;;; host MAKE-INSTANCE always with a class metaobject and never with a
;;; symbol.  If our version receives a symbol, it looks up the class
;;; metaobject in environment E1 before calling the host version.
;;;
;;; MAKE-INSTANCE is called by DEFINE-METHOD-COMBINATION to create a
;;; method-combination template, and by FIND-METHOD-COMBINATION to
;;; create a method-combination variant.  In phase 2, these are both
;;; defined in E1.
(defun define-make-instance-in-e1 (e1)
  (setf (sicl-genv:fdefinition 'make-instance e1)
        (lambda (class-or-name &rest args)
          (let* ((class (if (symbolp class-or-name)
                            (sicl-genv:find-class class-or-name e1)
                            class-or-name))
                 (result (apply #'make-instance class args)))
            result))))

;;; MAKE-INSTANCE is called in environment E2 when DEFMETHOD is called
;;; in environment E2 to create a method to add to a bridge generic
;;; function in E2.
(defun define-make-instance-in-e2 (e1 e2)
  (setf (sicl-genv:fdefinition 'make-instance e2)
        (sicl-genv:fdefinition 'make-instance e1)))

(defmethod sicl-genv:typep
    (object (type-specifier (eql 'class)) (environment environment))
  t)

(defmethod sicl-genv:typep
    (object (type-specifier (eql 'generic-function)) (environment environment))
  t)

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
  (load-file "CLOS/class-initialization-defmethods.lisp" e2)
  (define-add-accessor-method e1 e2 e3))

(defun boot-phase-2 (boot)
  (format *trace-output* "Start of phase 2~%")
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)
                   (e4 sicl-new-boot:e4)) boot
    (change-class e2 'environment)
    (import-function-from-host '(setf sicl-genv:special-variable) e2)
    (define-make-instance-in-e1 e1)
    (define-make-instance-in-e2 e1 e2)
    (enable-defmethod-in-e2 boot)
    (define-method-on-method-function e2)
    (load-accessor-defgenerics boot)
    ;; REMOVE is needed by the class initialization protocol.
    (import-function-from-host 'remove e2)
    (add-support-for-class-initialization-to-e2 e1 e2 e3)
    (import-function-from-host 'sicl-clos:defclass-expander e2)
    (create-mop-classes boot)))
