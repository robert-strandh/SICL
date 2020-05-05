(cl:in-package #:sicl-boot-phase-1)

(defun import-from-host (boot)
  (with-accessors ((e1 sicl-boot:e1))
      boot
    ;; Import class T so that it can be found when we need to create
    ;; the class T as a specializer for unspecialized method parameters.
    (setf (sicl-genv:find-class 't e1)
          (find-class 't))
    ;; Create classes in E1 that are potential metaclasses for the
    ;; classes we will create in phase 1.  It doesn't matter much how
    ;; these metaclasses are related by inheritance, so we just make
    ;; them all the same class, i.e. our special metaclass defined
    ;; above.
    (setf (sicl-genv:find-class 'standard-class e1)
          (find-class 'funcallable-standard-class))
    (setf (sicl-genv:find-class 'sicl-clos:funcallable-standard-class e1)
          (find-class 'funcallable-standard-class))
    (setf (sicl-genv:find-class 'sicl-clos:built-in-class e1)
          (find-class 'funcallable-standard-class))
    ;; We need to create methods on host generic functions such as
    ;; SHARED-INITIALIZE, so we need to instantiate the host class
    ;; STANDARD-METHOD.  For that reason, we import it from the host.
    (setf (sicl-genv:find-class 'standard-method e1)
          (find-class 'standard-method))
    ;; Import the class STANDARD-GENERIC-FUNCTION so that we can
    ;; create instance of it.
    (setf (sicl-genv:find-class 'standard-generic-function e1)
          (find-class 'standard-generic-function))
    ;; When we create class metaobjects, we need to instantiate the
    ;; class STANDARD-DIRECT-SLOT-DEFINITION, so we import it here.
    (setf (sicl-genv:find-class 'sicl-clos:standard-direct-slot-definition e1)
          (find-class 'closer-mop:standard-direct-slot-definition))))
