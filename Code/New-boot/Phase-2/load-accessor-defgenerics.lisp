(cl:in-package #:sicl-new-boot-phase-2)

(defclass header (closer-mop:funcallable-standard-object)
  ((%class :initarg :class)
   (%rack :initarg :rack))
  (:metaclass closer-mop:funcallable-standard-class))

(defun activate-allocate-instance (boot)
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
     '((setf sicl-genv:constant-variable) sort assoc list* every
       mapc 1+ 1- subseq butlast position identity nthcdr equal
       remove-if-not mapcar reverse find compile)
     e2)
    (load-file "CLOS/class-unique-number-offset-defconstant.lisp" e2)
    (load-file "CLOS/allocate-instance-defgenerics.lisp" e2)
    (load-file "CLOS/allocate-instance-support.lisp" e2)
    (load-file "CLOS/allocate-instance-defmethods.lisp" e2)))

(defun my-class-of (object)
  (if (typep object 'header)
      (slot-value object '%class)
      (class-of object)))

(defun activate-generic-function-invocation (boot)
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)) boot
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
          #'my-class-of)
    (setf (sicl-genv:fdefinition 'find-class e2)
          (lambda (name)
            (sicl-genv:find-class name e1)))
    (setf (sicl-genv:fdefinition 'sicl-clos:set-funcallable-instance-function e2)
          #'closer-mop:set-funcallable-instance-function)
    (load-file-protected "CLOS/list-utilities.lisp" e2)
    (load-file "CLOS/compute-applicable-methods-support.lisp" e2)
    (load-file "New-boot/Phase-3/sub-specializer-p.lisp" e2)
    (load-file "CLOS/compute-applicable-methods-defgenerics.lisp" e2)
    (load-file "CLOS/compute-applicable-methods-defmethods.lisp" e2)
    (import-package-from-host '#:sicl-method-combination e2)
    (import-function-from-host
     'sicl-method-combination::define-method-combination-expander e2)
    (load-file "Method-combination/define-method-combination-defmacro.lisp" e2)
    (import-functions-from-host
     '(sicl-genv:find-method-combination-template
       (setf sicl-genv:find-method-combination-template)
       sicl-loop::list-cdr sicl-loop::list-car)
     e2)
    (setf (sicl-genv:find-class
           'sicl-method-combination:method-combination-template e1)
          (find-class 'sicl-method-combination:method-combination-template))
    (load-file "CLOS/standard-method-combination.lisp" e2)
    (import-functions-from-host
     '(sicl-method-combination:find-method-combination
       sicl-method-combination:effective-method-form-function)
     e2)
    (load-file "CLOS/find-method-combination-defgenerics.lisp" e2)
    (load-file "CLOS/find-method-combination-defmethods.lisp" e2)
    (load-file "CLOS/compute-effective-method-defgenerics.lisp" e2)
    (load-file "CLOS/compute-effective-method-support.lisp" e2)
    (setf (sicl-genv:fdefinition 'make-method e2)
          (lambda (function)
            (funcall (sicl-genv:fdefinition 'make-instance e2)
                     (sicl-genv:find-class 'standard-method e1)
                     :function function
                     :lambda-list '(x &rest args)
                     :specializers (list (sicl-genv:find-class t e2)))))
    (load-file "CLOS/compute-effective-method-support-c.lisp" e2)
    (load-file "CLOS/compute-effective-method-defmethods-b.lisp" e2)
    (load-file "CLOS/no-applicable-method-defgenerics.lisp" e2)
    (load-file "CLOS/no-applicable-method.lisp" e2)
    (import-functions-from-host
     '(zerop nth intersection make-list caddr) e2)
    (load-file "CLOS/compute-discriminating-function-defgenerics.lisp" e2)
    (load-file "CLOS/compute-discriminating-function-support.lisp" e2)
    (import-functions-from-host
     '(sicl-clos::add-path
       sicl-clos::compute-discriminating-tagbody
       sicl-clos::extract-transition-information
       sicl-clos::make-automaton)
     e2)
    (load-file "CLOS/compute-discriminating-function-support-c.lisp" e2)
    (load-file "CLOS/compute-discriminating-function-defmethods.lisp" e2)
    (load-file-protected "CLOS/satiation.lisp" e2)
    (import-functions-from-host '(format print-object) e2)
    (load-file "New-boot/Phase-3/define-methods-on-print-object.lisp" e2)
    (load-file "New-boot/Phase-3/compute-and-set-specialier-profile.lisp" e2)
    (load-file "CLOS/standard-instance-access.lisp" e2)))

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

;;; FIXME: remove the :AROUND method after booting is complete.
(defun set-up-generic-function-initialization (boot)
  (let* ((temp (gensym))
         (class-env (sicl-new-boot:e1 boot))
         (gf-class-name 'generic-function)
         (gf-class (sicl-genv:find-class gf-class-name class-env)))
    (setf (find-class temp) gf-class)
    (eval `(defmethod shared-initialize :around
            ((generic-function ,temp)
             slot-names
             &rest initargs
             &key &allow-other-keys)
            (apply #'sicl-clos::shared-initialize-around-generic-function-default
             #'call-next-method
             #'identity
             generic-function
             slot-names
             initargs)))
    (setf (find-class temp) gf-class) nil))

(defun load-accessor-defgenerics (boot)
  (activate-allocate-instance boot)
  (activate-generic-function-invocation boot)
  (sicl-minimal-extrinsic-environment:host-load
   "CLOS/generic-function-initialization-support.lisp")
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3))
      boot
    (sicl-minimal-extrinsic-environment:import-function-from-host
     'sicl-clos:defgeneric-expander e3)
    (load-file "CLOS/defgeneric-defmacro.lisp" e3)
    (import-package-from-host 'sicl-method-combination e1)
    (import-class-from-host
     'sicl-method-combination:method-combination-template
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
    (set-up-generic-function-initialization boot)
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
    (load-file "CLOS/code-object-defgeneric.lisp" e3)))
