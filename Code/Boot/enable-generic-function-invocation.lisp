(cl:in-package #:sicl-boot)

;;; SUB-SPECIALIZER-P calls CLASS-PRECEDENCE-LIST to obtain the class
;;; precedence list of an argument passed to a generic function.  Then
;;; it calls POSITION to determine which of two classes comes first in
;;; that precedence list.
;;;
;;; SUB-SPECIALIZER-P is called by COMPUTE-APPLICABLE-METHODS
;;; (indirectly) to determine which is two methods is more specific.
(defun define-sub-specializer-p (environment)
  (load-source "CLOS/sub-specializer-p.lisp" environment))

(defun define-compute-applicable-methods (eb)
  (load-source "CLOS/compute-applicable-methods-support.lisp" eb)
  (load-source "CLOS/compute-applicable-methods-defgenerics.lisp" eb)
  (load-source "CLOS/compute-applicable-methods-defmethods.lisp" eb))

(defun define-compute-effective-method (eb)
  (load-source "CLOS/compute-effective-method-defgenerics.lisp" eb)
  (load-source "CLOS/compute-effective-method-support.lisp" eb)
  (load-source "CLOS/compute-effective-method-defmethods.lisp" eb))

(defun define-compute-discriminating-function (eb)
  (load-source "CLOS/compute-discriminating-function-defgenerics.lisp" eb)
  (load-source "CLOS/compute-discriminating-function-support.lisp" eb)
  (load-source "CLOS/discriminating-tagbody.lisp" eb)
  (load-source "CLOS/compute-discriminating-function-support-c.lisp" eb)
  (load-source "CLOS/compute-discriminating-function-defmethods.lisp" eb))

(defun define-no-applicable-method (eb)
  (load-source "CLOS/no-applicable-method-defgenerics.lisp" eb)
  (load-source "CLOS/no-applicable-method.lisp" eb))

(defun define-find-accessor-method-class (ea eb)
  (setf (sicl-genv:fdefinition 'sicl-clos::find-accessor-method-class eb)
        (lambda (class-name &optional error-p)
          (declare (ignore error-p))
          (assert (member class-name
                          '(sicl-clos:standard-reader-method
                            sicl-clos:standard-writer-method)))
          (sicl-genv:find-class class-name ea))))

(defun define-classp (eb)
  (load-source "CLOS/classp-defgeneric.lisp" eb)
  (load-source "CLOS/classp-defmethods.lisp" eb))

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
  (load-source "Evaluation-and-compilation/lambda.lisp" eb)
  (load-source "Data-and-control-flow/setf-defmacro.lisp" eb)
  (define-find-accessor-method-class ea eb)
  (define-compute-discriminating-function eb))
