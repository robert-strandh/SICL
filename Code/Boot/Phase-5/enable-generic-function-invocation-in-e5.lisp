(cl:in-package #:sicl-boot-phase-5)

;;; SUB-SPECIALIZER-P calls CLASS-PRECEDENCE-LIST to obtain the class
;;; precedence list of an argument passed to a generic function.  Then
;;; it calls POSITION to determine which of two classes comes first in
;;; that precedence list.
;;;
;;; SUB-SPECIALIZER-P is called by COMPUTE-APPLICABLE-METHODS
;;; (indirectly) to determine which is two methods is more specific.
(defun define-sub-specializer-p (environment)
  (import-function-from-host 'position environment)
  (load-file "Boot/Phase-2/sub-specializer-p.lisp" environment))

;;; COMPUTE-APPLICABLE-METHODS calls MAPCAR (indirectly) in order to
;;; get the class of each of the arguments passed to a generic
;;; function.  It calls SORT to sort the applicable methods in order
;;; from most specific to least specific.  EQL is called to compare
;;; the object of an EQL specializer to an argument passed to a
;;; generic function.
(defun define-compute-applicable-methods (e5)
  (import-functions-from-host '(sort mapcar eql) e5)
  (load-file "CLOS/compute-applicable-methods-support.lisp" e5)
  (load-file "CLOS/compute-applicable-methods-defgenerics.lisp" e5)
  (load-file "CLOS/compute-applicable-methods-defmethods.lisp" e5))
    
(defun define-compute-effective-method (e5)
  (load-file "CLOS/compute-effective-method-defgenerics.lisp" e5)
  (load-file "CLOS/compute-effective-method-support-c.lisp" e5)
  (load-file "CLOS/compute-effective-method-defmethods-b.lisp" e5))

(defun define-compute-discriminating-function (e5)
  (load-file "CLOS/compute-discriminating-function-defgenerics.lisp" e5)
  (load-file "CLOS/stamp-defun.lisp" e5)
  ;; LIST* is called in order to make a call cache.  CAR, CADR,
  ;; CADDR and CDDDR are used as accessors for the call cache.  FIND
  ;; is used to search a list of effictive-slot metaobjects to find
  ;; one with a particular name.  SUBSEQ is used to extract the
  ;; required arguments from a list of all the arguments to a
  ;; generic function.
  (import-functions-from-host '(list* car cadr caddr cdddr find subseq) e5)
  (load-file "CLOS/compute-discriminating-function-support.lisp" e5)
  (import-functions-from-host
   '(sicl-clos::add-path
     sicl-clos::compute-discriminating-tagbody
     sicl-clos::extract-transition-information
     sicl-clos::make-automaton)
   e5)
  ;; 1+ is called by COMPUTE-DISCRIMINATING-FUNCTION to compute an
  ;; argument for MAKE-AUTOMATON..
  (import-function-from-host '1+ e5)
  ;; NTH is called by COMPUTE-DISCRIMINATING-FUNCTION in order to
  ;; traverse the parameters that are specialized upon.
  (import-function-from-host 'nth e5)
  ;; ASSOC is used by COMPUTE-DISCRIMINATING-FUNCTION in order to
  ;; build a dictionary mapping effective-method functions to forms.
  (import-function-from-host 'assoc e5)
  (load-file "CLOS/compute-discriminating-function-support-c.lisp" e5)
  (load-file "CLOS/compute-discriminating-function-defmethods.lisp" e5))

(defun define-general-instance-access (boot)
  (with-accessors ((e4 sicl-boot:e4)
                   (e5 sicl-boot:e5)) boot
    (setf (sicl-genv:fdefinition 'sicl-clos::general-instance-p e5)
          (sicl-genv:fdefinition 'sicl-clos::general-instance-p e4))
    (setf (sicl-genv:fdefinition 'sicl-clos::general-instance-access e5)
          (sicl-genv:fdefinition 'sicl-clos::general-instance-access e4))
    (setf (sicl-genv:fdefinition '(setf sicl-clos::general-instance-access) e5)
          (sicl-genv:fdefinition '(setf sicl-clos::general-instance-access) e4))))

(defun define-compile (e5)
    (setf (sicl-genv:fdefinition 'compile e5)
          (lambda (name &optional definition)
            (assert (null name))
            (assert (not (null definition)))
            (cleavir-env:eval definition e5 e5))))

(defun define-no-applicable-method (e5)
  (load-file "CLOS/no-applicable-method-defgenerics.lisp" e5)
  (load-file "CLOS/no-applicable-method.lisp" e5))

(defun define-find-class (e4 e5)
  ;; We may regret having defined FIND-CLASS this way in E5.
  (setf (sicl-genv:fdefinition 'find-class e5)
        (lambda (class-name &optional error-p)
          (declare (ignore error-p))
          (sicl-genv:find-class class-name e4))))

(defun define-classp (e5)
  (load-file "CLOS/classp-defgeneric.lisp" e5)
  (load-file "CLOS/classp-defmethods.lisp" e5))

(defun define-set-funcallable-instance-function (e5)
  (setf (sicl-genv:fdefinition 'sicl-clos:set-funcallable-instance-function e5)
        #'closer-mop:set-funcallable-instance-function))

(defun enable-generic-function-invocation (boot)
  (with-accessors ((e4 sicl-boot:e4)
                   (e5 sicl-boot:e5)) boot
    (define-classp e5)
    (define-sub-specializer-p e5)
    (define-compute-applicable-methods e5)
    (define-compute-effective-method e5)
    (define-no-applicable-method e5)
    (define-general-instance-access boot)
    (define-set-funcallable-instance-function e5)
    (define-compile e5)
    (define-find-class e4 e5)
    (define-compute-discriminating-function e5)))
