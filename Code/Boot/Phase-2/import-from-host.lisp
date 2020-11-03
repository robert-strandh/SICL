(cl:in-package #:sicl-boot-phase-2)

(defun import-from-host (boot)
  (with-accessors ((e1 sicl-boot:e1))
      boot
    (let ((client (env:client e1)))
      ;; Import class T so that it can be found when we need to create
      ;; the class T as a specializer for unspecialized method parameters.
      (setf (env:find-class client e1 't)
            (find-class 't))
      ;; Create classes in E1 that are potential metaclasses for the
      ;; classes we will create in phase 2.  It doesn't matter much how
      ;; these metaclasses are related by inheritance, so we just make
      ;; them all the same class, i.e. our special metaclass defined
      ;; above.
      (setf (env:find-class client e1 'standard-class)
            (find-class 'funcallable-standard-class))
      (setf (env:find-class client e1 'sicl-clos:funcallable-standard-class)
            (find-class 'funcallable-standard-class))
      (setf (env:find-class client e1 'sicl-clos:built-in-class)
            (find-class 'funcallable-standard-class))
      ;; We need to create methods on host generic functions such as
      ;; SHARED-INITIALIZE, so we need to instantiate the host class
      ;; STANDARD-METHOD.  For that reason, we import it from the host.
      (setf (env:find-class client e1 'standard-method)
            (find-class 'standard-method))
      ;; Import the class STANDARD-GENERIC-FUNCTION so that we can
      ;; create instance of it.
      (setf (env:find-class client e1 'standard-generic-function)
            (find-class 'standard-generic-function))
      ;; When we create class metaobjects, we need to instantiate the
      ;; class STANDARD-DIRECT-SLOT-DEFINITION, so we import it here.
      (setf (env:find-class client e1 'sicl-clos:standard-direct-slot-definition)
            (find-class 'closer-mop:standard-direct-slot-definition)))))
