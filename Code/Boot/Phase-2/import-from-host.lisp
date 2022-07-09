(cl:in-package #:sicl-boot-phase-2)

(defun import-from-host (boot)
  (with-accessors ((e2 sicl-boot:e2))
      boot
    (let ((client (env:client e2)))
      ;; Import class T so that it can be found when we need to create
      ;; the class T as a specializer for unspecialized method parameters.
      (setf (env:find-class client e2 't)
            (find-class 't))
      ;; Create classes in E2 that are potential metaclasses for the
      ;; classes we will create in phase 2.  It doesn't matter much how
      ;; these metaclasses are related by inheritance, so we just make
      ;; them all the same class, i.e. our special metaclass defined
      ;; above.
      (setf (env:find-class client e2 'standard-class)
            (find-class 'funcallable-standard-class))
      (setf (env:find-class client e2 'sicl-clos:funcallable-standard-class)
            (find-class 'funcallable-standard-class))
      (setf (env:find-class client e2 'sicl-clos:built-in-class)
            (find-class 'funcallable-standard-class))
      (setf (env:find-class client e2 'structure-class)
            (find-class 'funcallable-standard-class))
      ;; We need to create methods on host generic functions such as
      ;; SHARED-INITIALIZE, so we need to instantiate the host class
      ;; STANDARD-METHOD.  For that reason, we import it from the host.
      (setf (env:find-class client e2 'standard-method)
            (find-class 'standard-method))
      ;; Import the class GENERIC-FUNCTION for the purpose of TYPEP.
      (setf (env:find-class client e2 'generic-function)
            (find-class 'generic-function))
      ;; Import the class SYMBOL for the purpose of TYPEP.
      (setf (env:find-class client e2 'symbol)
            (find-class 'symbol))
      ;; Import the class STANDARD-GENERIC-FUNCTION so that we can
      ;; create instance of it.
      (setf (env:find-class client e2 'standard-generic-function)
            (find-class 'standard-generic-function))
      ;; When we create class metaobjects, we need to instantiate the
      ;; class STANDARD-DIRECT-SLOT-DEFINITION, so we import it here.
      (setf (env:find-class client e2 'sicl-clos:standard-direct-slot-definition)
            (find-class 'closer-mop:standard-direct-slot-definition))
      (setf (env:find-class client e2 'sicl-clos:specializer)
            (find-class 'closer-mop:specializer))
      (setf (env:find-class client e2 'sicl-clos:forward-referenced-class)
            (find-class 'closer-mop:forward-referenced-class))
      (import-functions-from-host '(class-of add-method) e2)
      (setf (env:fdefinition client e2 'sicl-clos:class-precedence-list)
            #'closer-mop:class-precedence-list)
      (setf (env:fdefinition client e2 'sicl-clos:class-direct-superclasses)
            #'closer-mop:class-direct-superclasses))))
