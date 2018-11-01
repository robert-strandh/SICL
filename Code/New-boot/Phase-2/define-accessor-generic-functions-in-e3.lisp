(cl:in-package #:sicl-new-boot-phase-2)

(defclass header (closer-mop:funcallable-standard-object)
  ((%class :initarg :class)
   (%rack :initarg :rack))
  (:metaclass closer-mop:funcallable-standard-class))

(defun enable-allocate-instance (boot)
  (with-accessors ((e2 sicl-new-boot:e2)) boot
    (setf (sicl-genv:fdefinition 'sicl-clos::allocate-general-instance e2)
          (lambda (class size)
            (make-instance 'header
              :class class
              :rack (make-array size :initial-element 10000000))))
    (setf (sicl-genv:fdefinition 'sicl-clos::general-instance-access e2)
          (lambda (object location)
            (aref (slot-value object '%rack) location)))
    (setf (sicl-genv:fdefinition '(setf sicl-clos::general-instance-access) e2)
          (lambda (value object location)
            (setf (aref (slot-value object '%rack) location) value)))
    (import-functions-from-host
     '((setf sicl-genv:constant-variable) sort assoc every
       mapc 1+ 1- subseq butlast position identity nthcdr equal
       remove-if-not mapcar reverse find compile)
     e2)
    (load-file "CLOS/class-unique-number-offset-defconstant.lisp" e2)
    (load-file "CLOS/allocate-instance-defgenerics.lisp" e2)
    (load-file "CLOS/allocate-instance-support.lisp" e2)
    (load-file "CLOS/allocate-instance-defmethods.lisp" e2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Creating class accessor generic functions.

(defun ensure-generic-function-phase-2 (boot)
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e3 sicl-new-boot:e3))
      boot
    (let* ((class-env (sicl-new-boot:e1 boot))
           (gf-class-name 'standard-generic-function)
           (gf-class (sicl-genv:find-class gf-class-name class-env))
           (method-class-name 'standard-method)
           (method-class (sicl-genv:find-class method-class-name class-env))
           (target-env e3)
           (method-combination
             (funcall (sicl-genv:fdefinition
                       'sicl-method-combination:find-method-combination
                       e1)
                      'standard '() e1)))
      (setf (sicl-genv:fdefinition 'ensure-generic-function target-env)
            (lambda (function-name &rest arguments
                     &key environment
                     &allow-other-keys)
              (let ((args (copy-list arguments)))
                (loop while (remf args :environment))
                (if (sicl-genv:fboundp function-name environment)
                    (sicl-genv:fdefinition function-name environment)
                    (setf (sicl-genv:fdefinition function-name environment)
                          (apply #'make-instance gf-class
                                 :name function-name
                                 :method-class method-class
                                 :method-combination method-combination
                                 args)))))))))

(defun load-accessor-defgenerics (e3)
  (load-file "CLOS/specializer-direct-generic-functions-defgeneric.lisp" e3)
  (load-file "CLOS/setf-specializer-direct-generic-functions-defgeneric.lisp" e3)
  (load-file "CLOS/specializer-direct-methods-defgeneric.lisp" e3)
  (load-file "CLOS/setf-specializer-direct-methods-defgeneric.lisp" e3)
  (load-file "CLOS/eql-specializer-object-defgeneric.lisp" e3)
  (load-file "CLOS/unique-number-defgeneric.lisp" e3)
  (load-file "CLOS/class-name-defgeneric.lisp" e3)
  (load-file "CLOS/class-direct-subclasses-defgeneric.lisp" e3)
  (load-file "CLOS/setf-class-direct-subclasses-defgeneric.lisp" e3)
  (load-file "CLOS/class-direct-default-initargs-defgeneric.lisp" e3)
  (load-file "CLOS/documentation-defgeneric.lisp" e3)
  (load-file "CLOS/setf-documentation-defgeneric.lisp" e3)
  (load-file "CLOS/class-finalized-p-defgeneric.lisp" e3)
  (load-file "CLOS/setf-class-finalized-p-defgeneric.lisp" e3)
  (load-file "CLOS/class-precedence-list-defgeneric.lisp" e3)
  (load-file "CLOS/precedence-list-defgeneric.lisp" e3)
  (load-file "CLOS/setf-precedence-list-defgeneric.lisp" e3)
  (load-file "CLOS/instance-size-defgeneric.lisp" e3)
  (load-file "CLOS/setf-instance-size-defgeneric.lisp" e3)
  (load-file "CLOS/class-direct-slots-defgeneric.lisp" e3)
  (load-file "CLOS/class-direct-superclasses-defgeneric.lisp" e3)
  (load-file "CLOS/class-default-initargs-defgeneric.lisp" e3)
  (load-file "CLOS/setf-class-default-initargs-defgeneric.lisp" e3)
  (load-file "CLOS/class-slots-defgeneric.lisp" e3)
  (load-file "CLOS/setf-class-slots-defgeneric.lisp" e3)
  (load-file "CLOS/class-prototype-defgeneric.lisp" e3)
  (load-file "CLOS/setf-class-prototype-defgeneric.lisp" e3)
  (load-file "CLOS/dependents-defgeneric.lisp" e3)
  (load-file "CLOS/setf-dependents-defgeneric.lisp" e3)
  (load-file "CLOS/generic-function-name-defgeneric.lisp" e3)
  (load-file "CLOS/generic-function-lambda-list-defgeneric.lisp" e3)
  (load-file "CLOS/generic-function-argument-precedence-order-defgeneric.lisp" e3)
  (load-file "CLOS/generic-function-declarations-defgeneric.lisp" e3)
  (load-file "CLOS/generic-function-method-class-defgeneric.lisp" e3)
  (load-file "CLOS/generic-function-method-combination-defgeneric.lisp" e3)
  (load-file "CLOS/generic-function-methods-defgeneric.lisp" e3)
  (load-file "CLOS/setf-generic-function-methods-defgeneric.lisp" e3)
  (load-file "CLOS/initial-methods-defgeneric.lisp" e3)
  (load-file "CLOS/setf-initial-methods-defgeneric.lisp" e3)
  (load-file "CLOS/call-history-defgeneric.lisp" e3)
  (load-file "CLOS/setf-call-history-defgeneric.lisp" e3)
  (load-file "CLOS/specializer-profile-defgeneric.lisp" e3)
  (load-file "CLOS/setf-specializer-profile-defgeneric.lisp" e3)
  (load-file "CLOS/method-function-defgeneric.lisp" e3)
  (load-file "CLOS/method-generic-function-defgeneric.lisp" e3)
  (load-file "CLOS/setf-method-generic-function-defgeneric.lisp" e3)
  (load-file "CLOS/method-lambda-list-defgeneric.lisp" e3)
  (load-file "CLOS/method-specializers-defgeneric.lisp" e3)
  (load-file "CLOS/method-qualifiers-defgeneric.lisp" e3)
  (load-file "CLOS/accessor-method-slot-definition-defgeneric.lisp" e3)
  (load-file "CLOS/setf-accessor-method-slot-definition-defgeneric.lisp" e3)
  (load-file "CLOS/slot-definition-name-defgeneric.lisp" e3)
  (load-file "CLOS/slot-definition-allocation-defgeneric.lisp" e3)
  (load-file "CLOS/slot-definition-type-defgeneric.lisp" e3)
  (load-file "CLOS/slot-definition-initargs-defgeneric.lisp" e3)
  (load-file "CLOS/slot-definition-initform-defgeneric.lisp" e3)
  (load-file "CLOS/slot-definition-initfunction-defgeneric.lisp" e3)
  (load-file "CLOS/slot-definition-storage-defgeneric.lisp" e3)
  (load-file "CLOS/slot-definition-readers-defgeneric.lisp" e3)
  (load-file "CLOS/slot-definition-writers-defgeneric.lisp" e3)
  (load-file "CLOS/slot-definition-location-defgeneric.lisp" e3)
  (load-file "CLOS/setf-slot-definition-location-defgeneric.lisp" e3)
  (load-file "CLOS/variant-signature-defgeneric.lisp" e3)
  (load-file "CLOS/template-defgeneric.lisp" e3)
  (load-file "CLOS/code-object-defgeneric.lisp" e3))

(defun define-accessor-generic-functions (boot)
  (enable-allocate-instance boot)
  (enable-generic-function-invocation boot)
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3))
      boot
    (import-function-from-host 'sicl-clos:defgeneric-expander e3)
    (load-file "CLOS/defgeneric-defmacro.lisp" e3)
    (import-package-from-host 'sicl-method-combination e1)
    (import-class-from-host 'sicl-method-combination:method-combination-template
     e1)
    (import-functions-from-host
     '(sicl-loop::list-car sicl-loop::list-cdr equal reverse
       sicl-method-combination::define-method-combination-expander
       sicl-genv:find-method-combination-template
       (setf sicl-genv:find-method-combination-template))
     e1)
    (load-file "Method-combination/define-method-combination-defmacro.lisp" e1)
    (load-file "CLOS/standard-method-combination.lisp" e1)
    (import-functions-from-host
     '(sicl-method-combination::variant-signature-determiner
       sicl-method-combination::variants
       (setf sicl-method-combination::variants))
     e1)
    (setf (sicl-genv:fdefinition 'sicl-clos::template e1)
          (sicl-genv:fdefinition 'sicl-clos::template e2))
    (setf (sicl-genv:fdefinition 'sicl-clos::variant-signature e1)
          (sicl-genv:fdefinition 'sicl-clos::variant-signature e2))
    (load-file "Method-combination/find-method-combination.lisp" e1)
    (ensure-generic-function-phase-2 boot)
    (import-function-from-host 'shared-initialize e2)
    (import-function-from-host
     'cleavir-code-utilities:parse-generic-function-lambda-list e2)
    (import-function-from-host 'cleavir-code-utilities:required e2)
    (load-file "CLOS/invalidate-discriminating-function.lisp" e2)
    ;; MAKE-LIST is called from the :AROUND method on
    ;; SHARED-INITIALIZE specialized to GENERIC-FUNCTION.
    (import-function-from-host 'make-list e2)
    ;; SET-DIFFERENCE is called by the generic-function initialization
    ;; protocol to verify that the argument precedence order is a
    ;; permutation of the required arguments.
    (import-function-from-host 'set-difference e2)
    ;; STRINGP is called by the generic-function initialization
    ;; protocol to verify that the documentation is a string.
    (import-function-from-host 'stringp e2)
    (load-file "CLOS/generic-function-initialization-support.lisp" e2)
    (load-file "CLOS/generic-function-initialization-defmethods.lisp" e2)
    (load-accessor-defgenerics e3)))
