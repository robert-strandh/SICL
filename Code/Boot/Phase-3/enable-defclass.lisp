(cl:in-package #:sicl-boot-phase-3)

(defun define-add-remove-direct-subclass (e3)
  (load-source-file "CLOS/add-remove-direct-subclass-support.lisp" e3)
  (load-source-file "CLOS/add-remove-direct-subclass-defgenerics.lisp" e3)
  (load-source-file "CLOS/add-remove-direct-subclass-defmethods.lisp" e3))

(defun enable-class-initialization (e2 e3 e4)
  (define-add-remove-direct-subclass e3)
  ;; During bootstrapping, the DIRECT-SLOT-DEFINITION-CLASS always
  ;; returns STANDARD-DIRECT-SLOT-DEFINITION.
  (setf (env:fdefinition
         (env:client e3) e3 'sicl-clos:direct-slot-definition-class)
        (lambda (class &rest initargs)
          (declare (ignore class initargs))
          (env:find-class
           (env:client e2) e2 'sicl-clos:standard-direct-slot-definition)))
  (with-intercepted-function-cells
      (e3
       (find-class
        (list (lambda (name)
                (if (member name '(standard-object sicl-clos:funcallable-standard-object))
                    (env:find-class (env:client e3) e3 name)
                    (env:find-class (env:client e2) e2 name))))))
    (load-source-file "CLOS/default-superclasses-defgeneric.lisp" e3)
    (load-source-file "CLOS/default-superclasses-defmethods.lisp" e3))
  ;; This one is needed because we finalize built-in classes as soon
  ;; as they are initialized.  Perhaps we should extract the only
  ;; function that is used by this initialzation to a separate
  ;; component so that we don't have to pull in the entire
  ;; finalization protocol here.
  (load-source-file "CLOS/class-finalization-defgenerics.lisp" e3)
  (with-intercepted-function-cells
      (e3
       (make-instance (list #'make-instance)))
    (load-source-file "CLOS/class-finalization-support.lisp" e3))
  (with-intercepted-function-cells
      (e3
       (sicl-clos:effective-slot-definition-class
        (list (lambda (class)
                (declare (ignore class))
                (env:find-class (env:client e2) e2 'sicl-clos:standard-effective-slot-definition)))))
    (load-source-file "CLOS/class-finalization-defmethods.lisp" e3))
  (with-intercepted-function-cells
      (e3
       (find-class
        (list (lambda (name)
                (env:find-class (env:client e2) e2 name)))))
    (load-source-file "CLOS/reader-writer-method-class-support.lisp" e3)
    (load-source-file "CLOS/reader-writer-method-class-defgenerics.lisp" e3)
    (load-source-file "CLOS/reader-writer-method-class-defmethods.lisp" e3))
  (with-intercepted-function-cells
      (e3
       (make-instance (list #'make-instance))
       (ensure-generic-function
        (list (lambda (function-name &rest ignore)
                (declare (ignore ignore))
                (env:fdefinition (env:client e4) e4 function-name))))
       (find-class
        (list (lambda (name)
                (assert (eq name 't))
                (env:find-class (env:client e3) e3 't)))))
    (load-source-file "CLOS/add-accessor-method.lisp" e3))
  (with-intercepted-function-cells
      (e3
       (make-instance (list #'make-instance))
       ;; There is a test to verify that the initfunction of a
       ;; direct-default-initarg is a function, and it always is during
       ;; bootstrapping.
       (functionp (list (constantly t)))
       ;; There is a test to verify that each direct superclass is a class
       ;; and it always is during bootstrapping.
       (typep (list (constantly t)))
       ;; There is a call to VALIDATE-SUPERCLASS, and during
       ;; bootstrapping, it will always return true.
       (sicl-clos:validate-superclass (list (constantly t))))
    (load-source-file "CLOS/class-initialization-support.lisp" e3))
  ;; When direct superclasses are checked for validity, a battery of
  ;; mechanisms is required, but during bootstrapping, we don't need
  ;; those checks, so we just disable them.
  (setf (env:fdefinition (env:client e3) e3 'sicl-clos::check-direct-superclasses)
        (constantly nil))
  (with-intercepted-function-cells
      (e3
       (sicl-clos:method-function
        (list (lambda (method)
                (closer-mop:method-function method)))))
    (load-source-file "CLOS/class-initialization-defmethods.lisp" e3))
  (load-source-file "CLOS/reinitialize-instance-defgenerics.lisp" e3)
  (load-source-file "CLOS/reinitialize-instance-support.lisp" e3)
  (load-source-file "CLOS/reinitialize-instance-defmethods.lisp" e3))

;;; This function defines a version of ENSURE-CLASS to be used in
;;; phase 3.  The definition of ENSURE-CLASS is made in E4.  This
;;; version of ENSURE-CLASS defines a new class in E3.  It also uses
;;; E3 to find superclasses of the class to be defined.  E4 is the
;;; environment containing the generic functions to which slot reader
;;; and slot writer methods are to be added.  E2 is the environment
;;; that contains the metaclasses to instantiate.
(defun define-ensure-class (e2 e3 e4)
  (let ((client (env:client e2)))
    (setf (env:fdefinition client e4 'sicl-clos:ensure-class)
          (lambda (class-name
                   &key
                     direct-slots
                     ((:direct-superclasses direct-superclass-names))
                     name
                     ((:metaclass metaclass-name) 'sicl-clos:standard-class)
                   &allow-other-keys)
            ;; We should be called only for the expansion of DEFCLASS,
            ;; and we make sure that we always pass the name of a
            ;; metaclass, and never a class metaobject.
            (assert (symbolp metaclass-name))
            (let (;; In phase 3, the metaclass is a host class to be
                  ;; found in E2.
                  (metaclass (env:find-class client e2 metaclass-name))
                  ;; The direct superclasses, on the other hand, are to
                  ;; be found in the same environment as the one in
                  ;; which this class will be defined.
                  (direct-superclasses
                    (loop for name in direct-superclass-names
                          collect (env:find-class client e3 name))))
              (let ((class
                      (if (eq metaclass-name 'built-in-class)
                          (make-instance metaclass
                            :name (make-symbol (symbol-name name))
                            :direct-superclasses direct-superclasses)
                          (make-instance metaclass
                            :name (make-symbol (symbol-name name))
                            :direct-slots direct-slots
                            :direct-superclasses direct-superclasses))))
                (setf (env:find-class client e3 class-name) class)))))))

(defun enable-defclass (e2 e3 e4)
  (enable-class-initialization e2 e3 e4)
  (define-ensure-class e2 e3 e4))
