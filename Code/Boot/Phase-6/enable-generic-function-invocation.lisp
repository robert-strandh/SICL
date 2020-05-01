(cl:in-package #:sicl-boot-phase-6)

;;; SUB-SPECIALIZER-P calls CLASS-PRECEDENCE-LIST to obtain the class
;;; precedence list of an argument passed to a generic function.  Then
;;; it calls POSITION to determine which of two classes comes first in
;;; that precedence list.
;;;
;;; SUB-SPECIALIZER-P is called by COMPUTE-APPLICABLE-METHODS
;;; (indirectly) to determine which is two methods is more specific.
(defun define-sub-specializer-p (environment)
  (load-fasl "CLOS/sub-specializer-p.fasl" environment))

(defun define-compute-applicable-methods (eb)
  (load-fasl "CLOS/compute-applicable-methods-support.fasl" eb)
  (load-fasl "CLOS/compute-applicable-methods-defgenerics.fasl" eb)
  (load-fasl "CLOS/compute-applicable-methods-defmethods.fasl" eb))
    
(defun define-compute-effective-method (eb)
  (load-fasl "CLOS/compute-effective-method-defgenerics.fasl" eb)
  (load-fasl "CLOS/compute-effective-method-support.fasl" eb)
  (load-fasl "CLOS/compute-effective-method-defmethods.fasl" eb))

(defun define-compute-discriminating-function (eb)
  (load-fasl "CLOS/compute-discriminating-function-defgenerics.fasl" eb)
  (load-fasl "Cons/accessor-defuns.fasl" eb)
  (load-fasl "CLOS/compute-discriminating-function-support.fasl" eb)
  (load-fasl "CLOS/discriminating-tagbody.fasl" eb)
  (load-fasl "CLOS/compute-discriminating-function-support-c.fasl" eb)
  (load-fasl "CLOS/compute-discriminating-function-defmethods.fasl" eb))

(defun define-no-applicable-method (eb)
  (load-fasl "CLOS/no-applicable-method-defgenerics.fasl" eb)
  (load-fasl "CLOS/no-applicable-method.fasl" eb))

(defun define-find-accessor-method-class (ea eb)
  (setf (sicl-genv:fdefinition 'sicl-clos::find-accessor-method-class eb)
        (lambda (class-name &optional error-p)
          (declare (ignore error-p))
          (assert (member class-name
                          '(sicl-clos:standard-reader-method
                            sicl-clos:standard-writer-method)))
          (sicl-genv:find-class class-name ea))))

(defun define-classp (eb)
  (load-fasl "CLOS/classp-defgeneric.fasl" eb)
  (load-fasl "CLOS/classp-defmethods.fasl" eb))

(defun define-set-funcallable-instance-function (eb)
  (setf (sicl-genv:fdefinition 'sicl-clos:set-funcallable-instance-function eb)
        (lambda (funcallable-instance function)
          (closer-mop:set-funcallable-instance-function
           funcallable-instance
           function))))

(defun enable-generic-function-invocation (ea eb)
  (define-classp eb)
  (define-sub-specializer-p eb)
  (define-compute-applicable-methods eb)
  (define-compute-effective-method eb)
  (define-no-applicable-method eb)
  (define-set-funcallable-instance-function eb)
  (do-symbols (symbol (find-package '#:common-lisp))
    (when (special-operator-p symbol)
      (setf (sicl-genv:special-operator symbol eb) t)))
  (setf (sicl-genv:fdefinition 'slot-value eb)
        (lambda (&rest args)
          (declare (ignore args))
          (error "slot-value called")))
  (setf (sicl-genv:fdefinition '(setf slot-value) eb)
        (lambda (&rest args)
          (declare (ignore args))
          (error "(setf slot-value) called")))
  (load-fasl "Evaluation-and-compilation/lambda.fasl" eb)
  (load-fasl "Data-and-control-flow/setf-defmacro.fasl" eb)
  (define-find-accessor-method-class ea eb)
  (define-compute-discriminating-function eb))
