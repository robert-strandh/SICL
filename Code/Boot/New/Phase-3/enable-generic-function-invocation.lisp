(cl:in-package #:sicl-boot-phase-3)

;;; SUB-SPECIALIZER-P calls CLASS-PRECEDENCE-LIST to obtain the class
;;; precedence list of an argument passed to a generic function.  Then
;;; it calls POSITION to determine which of two classes comes first in
;;; that precedence list.
;;;
;;; SUB-SPECIALIZER-P is called by COMPUTE-APPLICABLE-METHODS
;;; (indirectly) to determine which is two methods is more specific.
(defun define-sub-specializer-p (environment)
  (import-function-from-host 'position environment)
  (load-fasl "Boot/Phase-2/sub-specializer-p.fasl" environment))

;;; COMPUTE-APPLICABLE-METHODS calls MAPCAR (indirectly) in order to
;;; get the class of each of the arguments passed to a generic
;;; function.  It calls SORT to sort the applicable methods in order
;;; from most specific to least specific.  EQL is called to compare
;;; the object of an EQL specializer to an argument passed to a
;;; generic function.
(defun define-compute-applicable-methods (e3)
  (import-functions-from-host '(sort mapcar eql) e3)
  (load-fasl "CLOS/compute-applicable-methods-support.fasl" e3)
  (load-fasl "CLOS/compute-applicable-methods-defgenerics.fasl" e3)
  (load-fasl "CLOS/compute-applicable-methods-defmethods.fasl" e3))

(defun define-compute-effective-method (e3)
  (load-fasl "CLOS/compute-effective-method-defgenerics.fasl" e3)
  (load-fasl "CLOS/compute-effective-method-support-c.fasl" e3)
  (load-fasl "CLOS/compute-effective-method-defmethods-b.fasl" e3))

(defun define-compute-discriminating-function (e3)
  (load-fasl "CLOS/compute-discriminating-function-defgenerics.fasl" e3)
  (load-fasl "CLOS/stamp-defun.fasl" e3)
  ;; LIST* is called in order to make a call cache.  CAR, CADR,
  ;; CADDR and CDDDR are used as accessors for the call cache.  FIND
  ;; is used to search a list of effictive-slot metaobjects to find
  ;; one with a particular name.  SUBSEQ is used to extract the
  ;; required arguments from a list of all the arguments to a
  ;; generic function.
  (import-functions-from-host '(list* car cadr caddr cdddr find subseq) e3)
  (load-fasl "CLOS/compute-discriminating-function-support.fasl" e3)
  (import-functions-from-host
   '(sicl-clos::add-path
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
  ;; ASSOC is used by COMPUTE-DISCRIMINATING-FUNCTION in order to
  ;; build a dictionary mapping effective-method functions to forms.
  (import-function-from-host 'assoc e3)
  (load-fasl "CLOS/compute-discriminating-function-support-c.fasl" e3)
  (load-fasl "CLOS/compute-discriminating-function-defmethods.fasl" e3))

(defun define-general-instance-access (e3)
  (setf (sicl-genv:fdefinition 'sicl-clos::general-instance-access e3)
        (lambda (object location)
          (aref (slot-value object '%rack) location)))
  (setf (sicl-genv:fdefinition '(setf sicl-clos::general-instance-access) e3)
        (lambda (value object location)
          (setf (aref (slot-value object '%rack) location) value))))

(defun define-no-applicable-method (e3)
  (load-fasl "CLOS/no-applicable-method-defgenerics.fasl" e3)
  (load-fasl "CLOS/no-applicable-method.fasl" e3))

(defun define-find-class (e2 e3)
  (setf (sicl-genv:fdefinition 'find-class e3)
        (lambda (name)
          (sicl-genv:find-class name e2))))

(defun define-classp (e3)
  (load-fasl "CLOS/classp-defgeneric.fasl" e3)
  (load-fasl "CLOS/classp-defmethods.fasl" e3))

(defun define-class-of (e3)
    (setf (sicl-genv:fdefinition 'class-of e3)
          (lambda (object)
            (let ((result (cond ((typep object 'header)
                                 (slot-value object '%class))
                                ((consp object)
                                 (sicl-genv:find-class 'cons e3))
                                ((symbolp object)
                                 (sicl-genv:find-class 'symbol e3))
                                ((integerp object)
                                 (sicl-genv:find-class 'fixnum e3))
                                ((streamp object)
                                 (sicl-genv:find-class 't e3))
                                (t
                                 (class-of object)))))
              result))))

(defun define-set-funcallable-instance-function (e3)
  (setf (sicl-genv:fdefinition 'sicl-clos:set-funcallable-instance-function e3)
        #'closer-mop:set-funcallable-instance-function))

(defun define-compile (e3)
  (setf (sicl-genv:fdefinition 'compile e3)
        (lambda (name &optional definition)
          (assert (null name))
          (assert (not (null definition)))
          (cleavir-env:eval definition e3 e3))))

(defun enable-generic-function-invocation (boot)
  (with-accessors ((e2 sicl-boot:e2)
                   (e3 sicl-boot:e3)) boot
    ;;    (import-package-from-host '#:sicl-conditions e3)
    (setf (sicl-genv:fdefinition 'sicl-clos::general-instance-p e3)
          (lambda (object)
            (typep object 'header)))
    (setf (sicl-genv:fdefinition 'typep e3)
          (lambda (object type-specifier)
            (sicl-genv:typep object type-specifier e3)))
    (import-function-from-host 'cleavir-code-utilities:proper-list-p e2)
    (import-functions-from-host '(endp car cdr null rplacd atom eq not) e2)
    (import-functions-from-host '(cadr cddr cdddr) e2)
    (define-classp e3)
    (define-class-of e3)
    (define-find-class e2 e3)
    (define-set-funcallable-instance-function e3)
    (define-sub-specializer-p e3)
    (define-compute-applicable-methods e3)
    (enable-method-combinations boot)
    (define-compute-effective-method e3)
    (define-no-applicable-method e3)
    (define-general-instance-access e3)
    (define-compile e3)
    (define-compute-discriminating-function e3)
    (import-functions-from-host '(print-object) e3)
    (load-fasl "Boot/Phase-2/define-methods-on-print-object.fasl" e3)
    (load-fasl "CLOS/standard-instance-access.fasl" e3)))
