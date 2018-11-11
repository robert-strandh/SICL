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
        (lambda (class-or-name &rest args)
          (let* ((class (if (symbolp class-or-name)
                            (sicl-genv:find-class class-or-name e1)
                            class-or-name))
                 (result (apply #'make-instance class args)))
            result))))

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

(defun boot-phase-2 (boot)
  (format *trace-output* "Start of phase 2~%")
  (define-method-on-print-object-for-ersatz-objects boot)
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)) boot
    (change-class e2 'environment)
    (import-function-from-host '(setf sicl-genv:special-variable) e2)
    (define-make-instance-in-e1 e1)
    (define-make-instance-in-e2 e1 e2)
    (enable-defmethod-in-e2 boot)
    (define-method-on-method-function e2)
    (enable-allocate-instance-in-e2 e2)
    (enable-generic-function-invocation boot)
    (define-accessor-generic-functions boot)
    (enable-class-initialization-in-e2 e1 e2 e3)
    (create-mop-classes boot)))
