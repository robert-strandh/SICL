(cl:in-package #:sicl-boot-phase-5)

;;; SUB-SPECIALIZER-P calls CLASS-PRECEDENCE-LIST to obtain the class
;;; precedence list of an argument passed to a generic function.  Then
;;; it calls POSITION to determine which of two classes comes first in
;;; that precedence list.
;;;
;;; SUB-SPECIALIZER-P is called by COMPUTE-APPLICABLE-METHODS
;;; (indirectly) to determine which is two methods is more specific.
(defun define-sub-specializer-p (environment)
  (load-fasl "CLOS/sub-specializer-p.fasl" environment))

;;; COMPUTE-APPLICABLE-METHODS calls MAPCAR (indirectly) in order to
;;; get the class of each of the arguments passed to a generic
;;; function.  It calls SORT to sort the applicable methods in order
;;; from most specific to least specific.  EQL is called to compare
;;; the object of an EQL specializer to an argument passed to a
;;; generic function.
(defun define-compute-applicable-methods (e5)
  (load-fasl "CLOS/compute-applicable-methods-support.fasl" e5)
  (load-fasl "CLOS/compute-applicable-methods-defgenerics.fasl" e5)
  (load-fasl "CLOS/compute-applicable-methods-defmethods.fasl" e5))
    
(defun define-compute-effective-method (e5)
  (load-fasl "CLOS/compute-effective-method-defgenerics.fasl" e5)
  (load-fasl "CLOS/compute-effective-method-support.fasl" e5)
  (load-fasl "CLOS/compute-effective-method-defmethods.fasl" e5))

(defun define-compute-discriminating-function (e5)
  (load-fasl "CLOS/compute-discriminating-function-defgenerics.fasl" e5)
  (load-fasl "Cons/accessor-defuns.fasl" e5)
  (load-fasl "CLOS/compute-discriminating-function-support.fasl" e5)
  (load-fasl "CLOS/discriminating-tagbody.fasl" e5)
  (load-fasl "CLOS/compute-discriminating-function-support-c.fasl" e5)
  (load-fasl "CLOS/compute-discriminating-function-defmethods.fasl" e5))

(defun define-general-instance-access (boot)
  (with-accessors ((e4 sicl-boot:e4)
                   (e5 sicl-boot:e5))
      boot
    (setf (sicl-genv:fdefinition 'sicl-clos::general-instance-p e5)
          (sicl-genv:fdefinition 'sicl-clos::general-instance-p e4))))

(defun define-no-applicable-method (e5)
  (load-fasl "CLOS/no-applicable-method-defgenerics.fasl" e5)
  (load-fasl "CLOS/no-applicable-method.fasl" e5))

(defun define-find-accessor-method-class (e4 e5)
  (setf (sicl-genv:fdefinition 'sicl-clos::find-accessor-method-class e5)
        (lambda (class-name &optional error-p)
          (declare (ignore error-p))
          (assert (member class-name
                          '(sicl-clos:standard-reader-method
                            sicl-clos:standard-writer-method)))
          (sicl-genv:find-class class-name e4))))

(defun define-classp (e5)
  (load-fasl "CLOS/classp-defgeneric.fasl" e5)
  (load-fasl "CLOS/classp-defmethods.fasl" e5))

(defun define-set-funcallable-instance-function (e5)
  (setf (sicl-genv:fdefinition 'sicl-clos:set-funcallable-instance-function e5)
        (lambda (funcallable-instance function)
          (closer-mop:set-funcallable-instance-function
           funcallable-instance
           function))))

(defun enable-generic-function-invocation (boot)
  (with-accessors ((e4 sicl-boot:e4)
                   (e5 sicl-boot:e5))
      boot
    (define-classp e5)
    (define-sub-specializer-p e5)
    (define-compute-applicable-methods e5)
    (define-compute-effective-method e5)
    (define-no-applicable-method e5)
    (define-general-instance-access boot)
    (define-set-funcallable-instance-function e5)
    (do-symbols (symbol (find-package '#:common-lisp))
      (when (special-operator-p symbol)
        (setf (sicl-genv:special-operator symbol e5) t)))
    (setf (sicl-genv:fdefinition 'slot-value e5)
          (lambda (&rest args)
            (declare (ignore args))
            (error "slot-value called")))
    (setf (sicl-genv:fdefinition '(setf slot-value) e5)
          (lambda (&rest args)
            (declare (ignore args))
            (error "(setf slot-value) called")))
    (load-fasl "Evaluation-and-compilation/lambda.fasl" e5)
    (load-fasl "Data-and-control-flow/setf-defmacro.fasl" e5)
    (define-find-accessor-method-class e4 e5)
    (define-compute-discriminating-function e5)))
