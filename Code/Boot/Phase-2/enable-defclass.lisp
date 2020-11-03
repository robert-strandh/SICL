(cl:in-package #:sicl-boot-phase-2)

;;; This function defines a version of ENSURE-CLASS to be used in
;;; phase 1.  The definition of ENSURE-CLASS is made in E3.  This
;;; version of ENSURE-CLASS defines a new class in E2.  It also uses
;;; E2 to find superclasses of the class to be defined.  E3 is the
;;; environment containing the generic functions to which slot reader
;;; and slot writer methods are to be added.  E1 is the environment
;;; that contains the metaclasses to instantiate.
(defun define-ensure-class (boot)
  (with-accessors ((e1 sicl-boot:e1)
                   (e2 sicl-boot:e2)
                   (e3 sicl-boot:e3))
      boot
    (let ((client (env:client e1)))
      (setf (env:fdefinition client e3 'sicl-clos:ensure-class)
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
              (let (;; In phase 2, the metaclass is a host class to be
                    ;; found in E1.
                    (metaclass (env:find-class client e1 metaclass-name))
                    ;; The direct superclasses, on the other hand, are to
                    ;; be found in the same environment as the one in
                    ;; which this class will be defined.
                    (direct-superclasses
                      (loop for name in direct-superclass-names
                            collect (env:find-class client e2 name))))
                (let ((class (make-instance metaclass
                               :name (make-symbol (symbol-name name))
                               :direct-slots direct-slots
                               :direct-superclasses direct-superclasses)))
                  (setf (env:find-class client e2 class-name) class))))))))

(defun enable-defclass (boot)
  (with-accessors ((e2 sicl-boot:e2)) boot
    ;; This function is needed because we define a special variable
    ;; *CLASS-UNIQUE-NUMBER* that is used in the :INITFORM of the 
    ;; corresponding slot in the class CLASS.
    ;; (sicl-boot:import-functions-from-host '((setf env:special-variable)) e2)
    (define-ensure-class boot)))
