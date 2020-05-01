(cl:in-package #:sicl-boot-phase-3)

;;; SUB-SPECIALIZER-P calls CLASS-PRECEDENCE-LIST to obtain the class
;;; precedence list of an argument passed to a generic function.  Then
;;; it calls POSITION to determine which of two classes comes first in
;;; that precedence list.
;;;
;;; SUB-SPECIALIZER-P is called by COMPUTE-APPLICABLE-METHODS
;;; (indirectly) to determine which is two methods is more specific.
(defun define-sub-specializer-p (environment)
  (load-fasl "CLOS/sub-specializer-p.fasl" environment))

(defun define-compute-applicable-methods (e3)
  (load-fasl "CLOS/compute-applicable-methods-support.fasl" e3)
  (load-fasl "CLOS/compute-applicable-methods-defgenerics.fasl" e3)
  (load-fasl "CLOS/compute-applicable-methods-defmethods.fasl" e3))

(defun define-compute-effective-method (e3)
  (load-fasl "CLOS/compute-effective-method-defgenerics.fasl" e3)
  (load-fasl "CLOS/compute-effective-method-support.fasl" e3)
  (load-fasl "CLOS/compute-effective-method-defmethods.fasl" e3))

(defun define-compute-discriminating-function (e3)
  (load-fasl "CLOS/compute-discriminating-function-defgenerics.fasl" e3)
  (load-fasl "Cons/accessor-defuns.fasl" e3)
  (load-fasl "CLOS/compute-discriminating-function-support.fasl" e3)
  (load-fasl "CLOS/discriminating-tagbody.fasl" e3)
  (load-fasl "CLOS/compute-discriminating-function-support-c.fasl" e3)
  (load-fasl "CLOS/compute-discriminating-function-defmethods.fasl" e3))

(defun define-no-applicable-method (e3)
  (load-fasl "CLOS/no-applicable-method-defgenerics.fasl" e3)
  (load-fasl "CLOS/no-applicable-method.fasl" e3))

(defun define-find-accessor-method-class (e2 e3)
  (setf (sicl-genv:fdefinition 'sicl-clos::find-accessor-method-class e3)
        (lambda (class-name)
          (assert (member class-name
                          '(sicl-clos:standard-reader-method
                            sicl-clos:standard-writer-method)))
          (sicl-genv:find-class class-name e2))))

(defun define-classp (e3)
  (load-fasl "CLOS/classp-defgeneric.fasl" e3)
  (load-fasl "CLOS/classp-defmethods.fasl" e3))

(defun define-set-funcallable-instance-function (e3)
  (setf (sicl-genv:fdefinition 'sicl-clos:set-funcallable-instance-function e3)
        #'closer-mop:set-funcallable-instance-function))

(defun enable-generic-function-invocation (boot)
  (with-accessors ((e2 sicl-boot:e2)
                   (e3 sicl-boot:e3)) boot
    (setf (sicl-genv:fdefinition 'typep e3)
          (lambda (object type-specifier)
            (sicl-genv:typep object type-specifier e3)))
    (load-fasl "Cons/accessor-defuns.fasl" e2)
    (load-fasl "Cons/cxr.fasl" e2)
    (define-classp e3)
    (define-find-accessor-method-class e2 e3)
    (define-set-funcallable-instance-function e3)
    (do-symbols (symbol (find-package '#:common-lisp))
      (when (special-operator-p symbol)
        (setf (sicl-genv:special-operator symbol e3) t)))
    (setf (sicl-genv:fdefinition 'slot-value e3)
          (lambda (&rest args)
            (declare (ignore args))
            (error "slot-value called")))
    (setf (sicl-genv:fdefinition '(setf slot-value) e3)
          (lambda (&rest args)
            (declare (ignore args))
            (error "(setf slot-value) called")))
    (define-sub-specializer-p e3)
    (define-compute-applicable-methods e3)
    (sicl-boot:enable-method-combinations #'load-fasl e2 e3)
    (define-compute-effective-method e3)
    (define-no-applicable-method e3)
    (load-fasl "Evaluation-and-compilation/lambda.fasl" e3)
    (load-fasl "Data-and-control-flow/setf-defmacro.fasl" e3)
    (define-compute-discriminating-function e3)
    (load-fasl "CLOS/standard-instance-access.fasl" e3)))
