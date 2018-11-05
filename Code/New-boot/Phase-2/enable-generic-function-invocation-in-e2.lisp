(cl:in-package #:sicl-new-boot-phase-2)

;;; COMPUTE-APPLICABLE-METHODS calls MAPCAR (indirectly) in order to
;;; get the class of each of the arguments passed to a generic
;;; function.  It calls SORT to sort the applicable methods in order
;;; from most specific to least specific.  EQL is called to compare
;;; the object of an EQL specializer to an argument passed to a
;;; generic function.
(defun define-compute-applicable-methods (e2)
  (import-functions-from-host '(sort mapcar eql) e2)
  (load-file "CLOS/compute-applicable-methods-support.lisp" e2)
  (load-file "CLOS/compute-applicable-methods-defgenerics.lisp" e2)
  (load-file "CLOS/compute-applicable-methods-defmethods.lisp" e2))

(defun define-compute-discriminating-function (e2)
  (load-file "CLOS/compute-discriminating-function-defgenerics.lisp" e2)
  ;; LIST* is called in order to make a call cache.  CAR, CADR,
  ;; CADDR and CDDDR are used as accessors for the call cache.  FIND
  ;; is used to search a list of effictive-slot metaobjects to find
  ;; one with a particular name.  SUBSEQ is used to extract the
  ;; required arguments from a list of all the arguments to a
  ;; generic function.
  (import-functions-from-host '(list* car cadr caddr cdddr find subseq) e2)
  (load-file "CLOS/compute-discriminating-function-support.lisp" e2)
  (import-functions-from-host
   '(sicl-clos::add-path
     sicl-clos::compute-discriminating-tagbody
     sicl-clos::extract-transition-information
     sicl-clos::make-automaton)
   e2)
  ;; 1+ is called by COMPUTE-DISCRIMINATING-FUNCTION to compute an
  ;; argument for MAKE-AUTOMATON..
  (import-function-from-host '1+ e2)
  ;; NTH is called by COMPUTE-DISCRIMINATING-FUNCTION in order to
  ;; traverse the parameters that are specialized upon.
  (import-function-from-host 'nth e2)
  (load-file "CLOS/compute-discriminating-function-support-c.lisp" e2)
  (load-file "CLOS/compute-discriminating-function-defmethods.lisp" e2))

(defun define-compute-effective-method (e2)
  (load-file "CLOS/compute-effective-method-defgenerics.lisp" e2)
  (load-file "CLOS/compute-effective-method-support-c.lisp" e2)
  (load-file "CLOS/compute-effective-method-defmethods-b.lisp" e2))

(defun enable-generic-function-invocation (boot)
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)) boot
    (import-package-from-host '#:sicl-conditions e2)
    (setf (sicl-genv:fdefinition 'sicl-clos::general-instance-p e2)
          (lambda (object)
            (typep object 'header)))
    (setf (sicl-genv:fdefinition 'typep e2)
          (lambda (object type-specifier)
            (sicl-genv:typep object type-specifier e2)))
    (load-file "CLOS/classp-defgeneric.lisp" e2)
    (load-file "CLOS/classp-defmethods.lisp" e2)
    (setf (sicl-genv:fdefinition 'class-of e2)
          (lambda (object)
            (let ((result (cond ((typep object 'header)
                                 (slot-value object '%class))
                                ((consp object)
                                 (sicl-genv:find-class 'cons e2))
                                ((symbolp object)
                                 (sicl-genv:find-class 't e2))
                                ((streamp object)
                                 (sicl-genv:find-class 't e2))
                                (t
                                 (class-of object)))))
              result)))
    (setf (sicl-genv:fdefinition 'find-class e2)
          (lambda (name)
            (sicl-genv:find-class name e1)))
    (setf (sicl-genv:fdefinition 'sicl-clos:set-funcallable-instance-function e2)
          #'closer-mop:set-funcallable-instance-function)
    (load-file "New-boot/Phase-2/sub-specializer-p.lisp" e2)
    (define-compute-applicable-methods e2)
    (enable-method-combination-in-e2 boot)
    (define-compute-effective-method e2)
    (load-file "CLOS/no-applicable-method-defgenerics.lisp" e2)
    (load-file "CLOS/no-applicable-method.lisp" e2)
    (define-compute-discriminating-function e2)
    (import-functions-from-host '(print-object) e2)
    (load-file "New-boot/Phase-2/define-methods-on-print-object.lisp" e2)
    (load-file "CLOS/standard-instance-access.lisp" e2)))
