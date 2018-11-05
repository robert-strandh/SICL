(cl:in-package #:sicl-new-boot-phase-3)

;;; SUB-SPECIALIZER-P ca calls CLASS-PRECEDENCE-LIST to obtain the
;;; class precedence list of an argument passed to a generic function.
;;; Then it calls POSITION to determine which of two classes comes
;;; first in that precedence list.
;;;
;;; SUB-SPECIALIZER-P is called by COMPUTE-APPLICABLE-METHODS
;;; (indirectly) to determine which is two methods is more specific.
(defun define-sub-specializer-p (boot)
  (with-accessors ((e3 sicl-new-boot:e3)) boot
    (import-function-from-host 'position e3)
    (load-file "New-boot/Phase-2/sub-specializer-p.lisp" e3)))

;;; COMPUTE-APPLICABLE-METHODS calls MAPCAR (indirectly) in order to
;;; get the class of each of the arguments passed to a generic
;;; function.  It calls SORT to sort the applicable methods in order
;;; from most specific to least specific.  EQL is called to compare
;;; the object of an EQL specializer to an argument passed to a
;;; generic function.
(defun define-compute-applicable-methods (e3)
  (import-functions-from-host '(sort mapcar eql) e3)
  (load-file "CLOS/compute-applicable-methods-support.lisp" e3)
  (load-file "CLOS/compute-applicable-methods-defgenerics.lisp" e3)
  (load-file "CLOS/compute-applicable-methods-defmethods.lisp" e3))
    
;;; COMPUTE-EFFECTIVE-METHOD calls several functions.  For each method
;;; that is passed to it as an argument, it calls METHOD-QUALIFIERS.
;;; It calls TEMPLATE on the method combination to get to the
;;; method-combination template.  It call VARIANT-SIGNATURE on the
;;; method combination to get to the variant signature of the method
;;; combination.  It calls EFFECTIVE-METHOD-FORM-FUNCTION on the
;;; template to get to the function to apply.  It calls
;;; GENERIC-FUNCTION-METHOD-CLASS on the generic function passed to it
;;; as an argument.  The functions METHOD-QUALIFIERS, TEMPLATE,
;;; VARIANT-SIGNATURE and GENERIC-FUNCTION-METHOD-CLASS are all
;;; accessor generic functions that were defined during phase 2 by
;;; evaluating DEFGENERIC forms and by evaluating DEFCLASS forms that
;;; define readers and writers on the generic functions.
(defun define-compute-effective-method (e3)
  (load-file "CLOS/compute-effective-method-defgenerics.lisp" e3)
  (load-file "CLOS/compute-effective-method-support-c.lisp" e3)
  (load-file "CLOS/compute-effective-method-defmethods-b.lisp" e3))

(defun define-general-instance-access (boot)
  (with-accessors ((e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)) boot
    (setf (sicl-genv:fdefinition 'sicl-clos::general-instance-p e3)
          (sicl-genv:fdefinition 'sicl-clos::general-instance-p e2))
    (setf (sicl-genv:fdefinition 'sicl-clos::general-instance-access e3)
          (sicl-genv:fdefinition 'sicl-clos::general-instance-access e2))
    (setf (sicl-genv:fdefinition '(setf sicl-clos::general-instance-access) e3)
          (sicl-genv:fdefinition '(setf sicl-clos::general-instance-access) e2))))

(defun define-compute-discriminating-function (e3)
  (load-file "CLOS/compute-discriminating-function-defgenerics.lisp" e3)
  ;; LIST* is called in order to make a call cache.  CAR, CADR,
  ;; CADDR and CDDDR are used as accessors for the call cache.  FIND
  ;; is used to search a list of effictive-slot metaobjects to find
  ;; one with a particular name.  SUBSEQ is used to extract the
  ;; required arguments from a list of all the arguments to a
  ;; generic function.
  (import-functions-from-host '(list* car cadr caddr cdddr find subseq) e3)
  (load-file "CLOS/compute-discriminating-function-support.lisp" e3)
  (import-functions-from-host
   '(assoc
     sicl-clos::add-path
     sicl-clos::compute-discriminating-tagbody
     sicl-clos::extract-transition-information
     sicl-clos::make-automaton)
   e3)
  ;; 1+ is called by COMPUTE-DISCRIMINATING-FUNCTION to compute an
  ;; argument for MAKE-AUTOMATON..
  (import-function-from-host '1+ e3)
  ;; NTH is called by COMPUTE-DISCRIMINATING-FUNCTION in order to
  ;; traverse the parameters that are specialized upon.
  (import-function-from-host 'nth e3)
  (load-file "CLOS/compute-discriminating-function-support-c.lisp" e3)
  (load-file "CLOS/compute-discriminating-function-defmethods.lisp" e3))

(defun enable-generic-function-invocation (boot)
  (with-accessors ((e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)) boot
    (load-file "CLOS/classp-defgeneric.lisp" e3)
    (load-file "CLOS/classp-defmethods.lisp" e3)
    (define-sub-specializer-p boot)
    (define-compute-applicable-methods e3)
    (define-compute-effective-method e3)
    (load-file "CLOS/no-applicable-method-defgenerics.lisp" e3)
    (load-file "CLOS/no-applicable-method.lisp" e3)
    (define-general-instance-access boot)
    (setf (sicl-genv:fdefinition 'sicl-clos:set-funcallable-instance-function e3)
          #'closer-mop:set-funcallable-instance-function)
    (setf (sicl-genv:fdefinition 'compile e3)
          (lambda (name &optional definition)
            (assert (null name))
            (assert (not (null definition)))
            (cleavir-env:eval definition e3 e3)))
    ;; We may regret having defined FIND-CLASS this way in E3.
    (setf (sicl-genv:fdefinition 'find-class e3)
          (lambda (class-name &optional error-p)
            (declare (ignore error-p))
            (sicl-genv:find-class class-name e2)))
    (define-compute-discriminating-function e3)))
