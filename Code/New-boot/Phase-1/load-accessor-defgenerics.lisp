(cl:in-package #:sicl-new-boot-phase-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Creating class accessor generic functions.
;;;
;;; There are different ways in which we can accomplish this task,
;;; given the constraint that it has to be done by loading DEFGENERIC
;;; forms corresponding to the class accessor generic functions.
;;;
;;; We obviously can not use the host definition of DEFGENERIC because
;;; it might clobber any existing host definition.  In particular,
;;; this is the case for class accessor functions that have names in
;;; the COMMON-LISP package, for instance CLASS-NAME.  Since we must
;;; supply our own definition of DEFGENERIC, we are free to do what we
;;; want.
;;;
;;; The way we have chosen to do it is to provide a specific
;;; definition of ENSURE-GENERIC-FUNCTION.  We do not want to use the
;;; ordinary SICL version of ENSURE-GENERIC-FUNCTION because it
;;; requires a battery of additional functionality in the form of
;;; other generic functions.  So to keep things simple, we supply a
;;; special bootstrapping version of it.
;;;
;;; We can rely entirely on the host to execute the generic-function
;;; initialization protocol.  There is a little question about the
;;; initialization argument :METHOD-COMBINATION, but it appears that
;;; it defaults to the standard method combination, so we do not have
;;; to pass a method-combination metaobject here.

(defun ensure-generic-function-phase-1
    (function-name &rest arguments &key environment &allow-other-keys)
  (let ((args (copy-list arguments)))
    (loop while (remf args :environment))
    (if (sicl-genv:fboundp function-name environment)
        (sicl-genv:fdefinition function-name environment)
        (setf (sicl-genv:fdefinition function-name environment)
              (apply #'make-instance 'standard-generic-function
                     :name function-name
                     args)))))

(defun load-accessor-defgenerics (environment)
  (sicl-minimal-extrinsic-environment:import-function-from-host
   'sicl-clos:defgeneric-expander environment)
  (load-file "CLOS/defgeneric-defmacro.lisp" environment)
  (setf (sicl-genv:fdefinition 'ensure-generic-function environment)
        #'ensure-generic-function-phase-1)
  (load-file "CLOS/specializer-direct-generic-functions-defgeneric.lisp" environment)
  (load-file "CLOS/setf-specializer-direct-generic-functions-defgeneric.lisp" environment)
  (load-file "CLOS/specializer-direct-methods-defgeneric.lisp" environment)
  (load-file "CLOS/setf-specializer-direct-methods-defgeneric.lisp" environment)
  (load-file "CLOS/eql-specializer-object-defgeneric.lisp" environment)
  (load-file "CLOS/unique-number-defgeneric.lisp" environment)
  (load-file "CLOS/class-name-defgeneric.lisp" environment)
  (load-file "CLOS/class-direct-subclasses-defgeneric.lisp" environment)
  (load-file "CLOS/setf-class-direct-subclasses-defgeneric.lisp" environment)
  (load-file "CLOS/class-direct-default-initargs-defgeneric.lisp" environment)
  (load-file "CLOS/documentation-defgeneric.lisp" environment)
  (load-file "CLOS/setf-documentation-defgeneric.lisp" environment)
  (load-file "CLOS/class-finalized-p-defgeneric.lisp" environment)
  (load-file "CLOS/setf-class-finalized-p-defgeneric.lisp" environment)
  (load-file "CLOS/class-precedence-list-defgeneric.lisp" environment)
  (load-file "CLOS/precedence-list-defgeneric.lisp" environment)
  (load-file "CLOS/setf-precedence-list-defgeneric.lisp" environment)
  (load-file "CLOS/instance-size-defgeneric.lisp" environment)
  (load-file "CLOS/setf-instance-size-defgeneric.lisp" environment)
  (load-file "CLOS/class-direct-slots-defgeneric.lisp" environment)
  (load-file "CLOS/class-direct-superclasses-defgeneric.lisp" environment)
  (load-file "CLOS/class-default-initargs-defgeneric.lisp" environment)
  (load-file "CLOS/setf-class-default-initargs-defgeneric.lisp" environment)
  (load-file "CLOS/class-slots-defgeneric.lisp" environment)
  (load-file "CLOS/setf-class-slots-defgeneric.lisp" environment)
  (load-file "CLOS/class-prototype-defgeneric.lisp" environment)
  (load-file "CLOS/setf-class-prototype-defgeneric.lisp" environment)
  (load-file "CLOS/dependents-defgeneric.lisp" environment)
  (load-file "CLOS/setf-dependents-defgeneric.lisp" environment)
  (load-file "CLOS/generic-function-name-defgeneric.lisp" environment)
  (load-file "CLOS/generic-function-lambda-list-defgeneric.lisp" environment)
  (load-file "CLOS/generic-function-argument-precedence-order-defgeneric.lisp" environment)
  (load-file "CLOS/generic-function-declarations-defgeneric.lisp" environment)
  (load-file "CLOS/generic-function-method-class-defgeneric.lisp" environment)
  (load-file "CLOS/generic-function-method-combination-defgeneric.lisp" environment)
  (load-file "CLOS/generic-function-methods-defgeneric.lisp" environment)
  (load-file "CLOS/setf-generic-function-methods-defgeneric.lisp" environment)
  (load-file "CLOS/initial-methods-defgeneric.lisp" environment)
  (load-file "CLOS/setf-initial-methods-defgeneric.lisp" environment)
  (load-file "CLOS/call-history-defgeneric.lisp" environment)
  (load-file "CLOS/setf-call-history-defgeneric.lisp" environment)
  (load-file "CLOS/specializer-profile-defgeneric.lisp" environment)
  (load-file "CLOS/setf-specializer-profile-defgeneric.lisp" environment)
  (load-file "CLOS/method-function-defgeneric.lisp" environment)
  (load-file "CLOS/method-generic-function-defgeneric.lisp" environment)
  (load-file "CLOS/setf-method-generic-function-defgeneric.lisp" environment)
  (load-file "CLOS/method-lambda-list-defgeneric.lisp" environment)
  (load-file "CLOS/method-specializers-defgeneric.lisp" environment)
  (load-file "CLOS/method-qualifiers-defgeneric.lisp" environment)
  (load-file "CLOS/accessor-method-slot-definition-defgeneric.lisp" environment)
  (load-file "CLOS/setf-accessor-method-slot-definition-defgeneric.lisp" environment)
  (load-file "CLOS/slot-definition-name-defgeneric.lisp" environment)
  (load-file "CLOS/slot-definition-allocation-defgeneric.lisp" environment)
  (load-file "CLOS/slot-definition-type-defgeneric.lisp" environment)
  (load-file "CLOS/slot-definition-initargs-defgeneric.lisp" environment)
  (load-file "CLOS/slot-definition-initform-defgeneric.lisp" environment)
  (load-file "CLOS/slot-definition-initfunction-defgeneric.lisp" environment)
  (load-file "CLOS/slot-definition-storage-defgeneric.lisp" environment)
  (load-file "CLOS/slot-definition-readers-defgeneric.lisp" environment)
  (load-file "CLOS/slot-definition-writers-defgeneric.lisp" environment)
  (load-file "CLOS/slot-definition-location-defgeneric.lisp" environment)
  (load-file "CLOS/setf-slot-definition-location-defgeneric.lisp" environment)
  (load-file "CLOS/operation-defgeneric.lisp" environment)
  (load-file "CLOS/variant-signature-defgeneric.lisp" environment)
  (load-file "CLOS/template-defgeneric.lisp" environment))
