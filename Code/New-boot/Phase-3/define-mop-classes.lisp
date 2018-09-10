(cl:in-package #:sicl-new-boot-phase-3)

(defmethod sicl-genv:typep
    (object (type-specifier (eql 'function)) (environment environment))
  (typep object 'function))

;;; We only check for the type CLASS for reasons of catching errors,
;;; but during bootstrapping, we completely control the arguments, so
;;; we can simply return true here.
(defmethod sicl-genv:typep
    (object (type-specifier (eql 'class)) (environment environment))
  t)

(defun create-mop-classes (boot)
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)
                   (e4 sicl-new-boot:e4))
      boot
    (import-functions-from-host '(sicl-genv:typep) e3)
    (setf (sicl-genv:fdefinition 'sicl-clos:validate-superclass e3)
          (lambda (class direct-superclass)
            (declare (ignore class direct-superclass))
            t))
    nil))
