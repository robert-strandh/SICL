(cl:in-package #:sicl-boot-phase-4)

;;; SUB-SPECIALIZER-P calls CLASS-PRECEDENCE-LIST to obtain the class
;;; precedence list of an argument passed to a generic function.  Then
;;; it calls POSITION to determine which of two classes comes first in
;;; that precedence list.
;;;
;;; SUB-SPECIALIZER-P is called by COMPUTE-APPLICABLE-METHODS
;;; (indirectly) to determine which is two methods is more specific.
(defun define-sub-specializer-p (environment)
  (import-function-from-host 'position environment)
  (load-fasl "Boot/Phase-3/sub-specializer-p.fasl" environment))

;;; COMPUTE-APPLICABLE-METHODS calls MAPCAR (indirectly) in order to
;;; get the class of each of the arguments passed to a generic
;;; function.  It calls SORT to sort the applicable methods in order
;;; from most specific to least specific.  EQL is called to compare
;;; the object of an EQL specializer to an argument passed to a
;;; generic function.
(defun define-compute-applicable-methods (e4)
  (import-functions-from-host '(sort mapcar eql) e4)
  (load-fasl "CLOS/compute-applicable-methods-support.fasl" e4)
  (load-fasl "CLOS/compute-applicable-methods-defgenerics.fasl" e4)
  (load-fasl "CLOS/compute-applicable-methods-defmethods.fasl" e4))
    
(defun define-compute-effective-method (e4)
  (load-fasl "CLOS/compute-effective-method-defgenerics.fasl" e4)
  (load-fasl "CLOS/compute-effective-method-support-c.fasl" e4)
  (load-fasl "CLOS/compute-effective-method-defmethods-b.fasl" e4))

(defun define-compute-discriminating-function (e4)
  (load-fasl "CLOS/compute-discriminating-function-defgenerics.fasl" e4)
  (load-fasl "CLOS/stamp-defun.fasl" e4)
  ;; LIST* is called in order to make a call cache.  CAR, CADR,
  ;; CADDR and CDDDR are used as accessors for the call cache.  FIND
  ;; is used to search a list of effictive-slot metaobjects to find
  ;; one with a particular name.  SUBSEQ is used to extract the
  ;; required arguments from a list of all the arguments to a
  ;; generic function.
  (import-functions-from-host '(list* car cadr caddr cdddr find subseq) e4)
  (load-fasl "CLOS/compute-discriminating-function-support.fasl" e4)
  (import-functions-from-host
   '(sicl-clos::add-path
     sicl-clos::compute-discriminating-tagbody
     sicl-clos::extract-transition-information
     sicl-clos::make-automaton)
   e4)
  ;; 1+ is called by COMPUTE-DISCRIMINATING-FUNCTION to compute an
  ;; argument for MAKE-AUTOMATON..
  (import-function-from-host '1+ e4)
  ;; NTH is called by COMPUTE-DISCRIMINATING-FUNCTION in order to
  ;; traverse the parameters that are specialized upon.
  (import-function-from-host 'nth e4)
  ;; ASSOC is used by COMPUTE-DISCRIMINATING-FUNCTION in order to
  ;; build a dictionary mapping effective-method functions to forms.
  (import-function-from-host 'assoc e4)
  (load-fasl "CLOS/compute-discriminating-function-support-c.fasl" e4)
  (load-fasl "CLOS/compute-discriminating-function-defmethods.fasl" e4))

(defun define-general-instance-access (boot)
  (with-accessors ((e3 sicl-boot:e3)
                   (e4 sicl-boot:e4)) boot
    (setf (sicl-genv:fdefinition 'sicl-clos::general-instance-p e4)
          (sicl-genv:fdefinition 'sicl-clos::general-instance-p e3))
    (setf (sicl-genv:fdefinition 'sicl-clos::general-instance-access e4)
          (sicl-genv:fdefinition 'sicl-clos::general-instance-access e3))
    (setf (sicl-genv:fdefinition '(setf sicl-clos::general-instance-access) e4)
          (sicl-genv:fdefinition '(setf sicl-clos::general-instance-access) e3))))

(defun define-compile (e4)
  (setf (sicl-genv:fdefinition 'compile e4)
        (lambda (name &optional definition)
          (assert (null name))
          (assert (not (null definition)))
          (let* ((cst (cst:cst-from-expression definition))
                 (client (make-instance 'sicl-boot-phase-0:client)))
            (sicl-hir-interpreter:cst-eval client cst e4)))))

(defun define-no-applicable-method (e4)
  (load-fasl "CLOS/no-applicable-method-defgenerics.fasl" e4)
  (load-fasl "CLOS/no-applicable-method.fasl" e4))

(defun define-find-class (e3 e4)
  ;; We may regret having defined FIND-CLASS this way in E4.
  (setf (sicl-genv:fdefinition 'find-class e4)
        (lambda (class-name &optional error-p)
          (declare (ignore error-p))
          (sicl-genv:find-class class-name e3))))

(defun define-classp (e4)
  (load-fasl "CLOS/classp-defgeneric.fasl" e4)
  (load-fasl "CLOS/classp-defmethods.fasl" e4))

(defun define-set-funcallable-instance-function (e4)
  (setf (sicl-genv:fdefinition 'sicl-clos:set-funcallable-instance-function e4)
        #'closer-mop:set-funcallable-instance-function))

(defun enable-generic-function-invocation (boot)
  (with-accessors ((e3 sicl-boot:e3)
                   (e4 sicl-boot:e4)) boot
    (define-classp e4)
    (define-sub-specializer-p e4)
    (define-compute-applicable-methods e4)
    (define-compute-effective-method e4)
    (define-no-applicable-method e4)
    (define-general-instance-access boot)
    (define-set-funcallable-instance-function e4)
    (import-functions-from-host
     '((setf sicl-genv:fdefinition)
       (setf sicl-genv:macro-function)
       sicl-genv:macro-function
       sicl-genv:get-setf-expansion
       cadr consp = first)
     e4)
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
    (define-compile e4)
    (load-fasl "Evaluation-and-compilation/lambda.fasl" e4)
    (load-fasl "Data-and-control-flow/setf.fasl" e4)
    (define-find-class e3 e4)
    (define-compute-discriminating-function e4)
    (import-functions-from-host '(apply endp cons eq coerce) e4)))
