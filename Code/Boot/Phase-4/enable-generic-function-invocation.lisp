(cl:in-package #:sicl-boot-phase-4)

;;; SUB-SPECIALIZER-P calls CLASS-PRECEDENCE-LIST to obtain the class
;;; precedence list of an argument passed to a generic function.  Then
;;; it calls POSITION to determine which of two classes comes first in
;;; that precedence list.
;;;
;;; SUB-SPECIALIZER-P is called by COMPUTE-APPLICABLE-METHODS
;;; (indirectly) to determine which is two methods is more specific.
(defun define-sub-specializer-p (environment)
  (load-fasl "CLOS/sub-specializer-p.fasl" environment))

(defun define-compute-applicable-methods (e4)
  (load-fasl "CLOS/compute-applicable-methods-support.fasl" e4)
  (load-fasl "CLOS/compute-applicable-methods-defgenerics.fasl" e4)
  (load-fasl "CLOS/compute-applicable-methods-defmethods.fasl" e4))
    
(defun define-compute-effective-method (e4)
  (load-fasl "CLOS/compute-effective-method-defgenerics.fasl" e4)
  (load-fasl "CLOS/compute-effective-method-support.fasl" e4)
  (load-fasl "CLOS/compute-effective-method-defmethods.fasl" e4))

(defun define-compute-discriminating-function (e4)
  (load-fasl "CLOS/compute-discriminating-function-defgenerics.fasl" e4)
  (load-fasl "Cons/accessor-defuns.fasl" e4)
  (load-fasl "CLOS/compute-discriminating-function-support.fasl" e4)
  (load-fasl "CLOS/discriminating-tagbody.fasl" e4)
  (load-fasl "CLOS/compute-discriminating-function-support-c.fasl" e4)
  (load-fasl "CLOS/compute-discriminating-function-defmethods.fasl" e4))

(defun define-general-instance-access (boot)
  (with-accessors ((e3 sicl-boot:e3)
                   (e4 sicl-boot:e4))
      boot
    (setf (sicl-genv:fdefinition 'sicl-clos::general-instance-p e4)
          (sicl-genv:fdefinition 'sicl-clos::general-instance-p e3))))

(defun define-no-applicable-method (e4)
  (load-fasl "CLOS/no-applicable-method-defgenerics.fasl" e4)
  (load-fasl "CLOS/no-applicable-method.fasl" e4))

(defun define-find-accessor-method-class (e3 e4)
  (setf (sicl-genv:fdefinition 'sicl-clos::find-accessor-method-class e4)
        (lambda (class-name &optional error-p)
          (declare (ignore error-p))
          (assert (member class-name
                          '(sicl-clos:standard-reader-method
                            sicl-clos:standard-writer-method)))
          (sicl-genv:find-class class-name e3))))

(defun define-classp (e4)
  (load-fasl "CLOS/classp-defgeneric.fasl" e4)
  (load-fasl "CLOS/classp-defmethods.fasl" e4))

(defun define-set-funcallable-instance-function (e4)
  (setf (sicl-genv:fdefinition 'sicl-clos:set-funcallable-instance-function e4)
        (lambda (funcallable-instance function)
          (closer-mop:set-funcallable-instance-function
           funcallable-instance
           function))))

(defun enable-generic-function-invocation (boot)
  (with-accessors ((e3 sicl-boot:e3)
                   (e4 sicl-boot:e4))
      boot
    (define-classp e4)
    (define-sub-specializer-p e4)
    (define-compute-applicable-methods e4)
    (define-compute-effective-method e4)
    (define-no-applicable-method e4)
    (define-general-instance-access boot)
    (define-set-funcallable-instance-function e4)
    (do-symbols (symbol (find-package '#:common-lisp))
      (when (special-operator-p symbol)
        (setf (sicl-genv:special-operator symbol e4) t)))
    (setf (sicl-genv:fdefinition 'slot-value e4)
          (lambda (&rest args)
            (declare (ignore args))
            (error "slot-value called")))
    (setf (sicl-genv:fdefinition '(setf slot-value) e4)
          (lambda (&rest args)
            (declare (ignore args))
            (error "(setf slot-value) called")))
    (load-fasl "Evaluation-and-compilation/lambda.fasl" e4)
    (load-fasl "Data-and-control-flow/setf-defmacro.fasl" e4)
    (define-find-accessor-method-class e3 e4)
    (define-compute-discriminating-function e4)))
