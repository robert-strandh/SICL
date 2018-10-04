(cl:in-package #:sicl-new-boot-phase-0)

;;; This class will be the metaclass of all the MOP classes defined in
;;; E1.  We define it as a subclass of the host
;;; FUNCALLABLE-STANDARD-CLASS so that instances of it can be used as
;;; functions, and so that we avoid any problems with incompatible
;;; superclasses and metaclasses.
(defclass funcallable-standard-class
    (closer-mop:funcallable-standard-class)
  ())

(defun import-from-host (boot)
  (with-accessors ((e0 sicl-new-boot:e0)) boot
    (import-package-from-host 'sicl-clos e0)
    (import-functions-from-host
     '(sicl-genv:find-class)
     e0)
    (setf (sicl-genv:find-class 'standard-class e0)
          (find-class 'funcallable-standard-class))
    (setf (sicl-genv:find-class 'sicl-clos:funcallable-standard-class e0)
          (find-class 'funcallable-standard-class))
    (setf (sicl-genv:find-class 'sicl-clos:built-in-class e0)
          (find-class 'funcallable-standard-class))
    (setf (sicl-genv:find-class 'sicl-clos:standard-direct-slot-definition e0)
          (find-class 'closer-mop:standard-direct-slot-definition))))

(defun boot-phase-0 (boot)
  (import-from-host boot)
  (with-accessors ((e0 sicl-new-boot:e0) (e1 sicl-new-boot:e1)) boot
    nil))
