(cl:in-package #:sicl-new-boot-phase-2)


(defun define-method-on-print-object-for-ersatz-objects (boot)
  (with-accessors ((e3 sicl-new-boot:e3)) boot
    (defmethod print-object ((object header) stream)
      (funcall (sicl-genv:fdefinition 'print-object e3)
               object stream))))

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
          (sicl-genv:find-class 'sicl-clos:standard-writer-method e1)))
  (setf (sicl-genv:fdefinition 'compile e2)
        (lambda (name &optional definition)
          (assert (null name))
          (assert (not (null definition)))
          (cleavir-env:eval definition e2 e2)))
  (import-functions-from-host '(find-if adjoin set-exclusive-or) e2)
  (define-error-function 'slot-value e2)
  (define-error-function '(setf slot-value) e2)
  (setf (sicl-genv:fdefinition 'sicl-clos:add-direct-method e2)
        (lambda (&rest args) (declare (ignore args)) nil))
  (setf (sicl-genv:fdefinition 'sicl-clos:remove-direct-method e2)
        (lambda (&rest args) (declare (ignore args)) nil))
  (setf (sicl-genv:fdefinition 'sicl-clos:map-dependents e2)
        (lambda (&rest args) (declare (ignore args)) nil))
  (setf (sicl-genv:fdefinition 'sicl-clos:update-dependent e2)
        (lambda (&rest args) (declare (ignore args)) nil))
    (load-file "CLOS/add-remove-method-defgenerics.lisp" e2)
  (load-file-protected "CLOS/add-remove-method-support.lisp" e2)
  (load-file "CLOS/add-remove-method-defmethods.lisp" e2)
  (setf (sicl-genv:special-variable 'sicl-clos::*class-t* e2 nil) nil)
  (load-file "CLOS/add-accessor-method.lisp" e2)
  (setf (sicl-genv:fdefinition 'sicl-clos::find-or-create-generic-function e2)
        (lambda (name lambda-list)
          (declare (ignore lambda-list))
          (sicl-genv:fdefinition name e3)))
  (setf (sicl-genv:fdefinition 'sicl-clos:default-superclasses e2)
        (lambda (class) (declare (ignore class)) '()))
  (import-function-from-host 'sicl-genv:typep e2)
  (load-file "CLOS/class-initialization-support.lisp" e2)
  (load-file "CLOS/class-initialization-defmethods.lisp" e2)
  (sicl-genv:fmakunbound 'shared-initialize e2))

(defun boot-phase-2 (boot)
  (format *trace-output* "Start of phase 2~%")
  (define-method-on-print-object-for-ersatz-objects boot)
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
