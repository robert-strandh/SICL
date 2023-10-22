(cl:in-package #:sicl-new-boot-phase-1)

(defgeneric my-make-instance (class &rest initargs))

(defun define-make-instance (client environment)

  (defmethod my-make-instance ((name symbol) &rest initargs)
    (let ((class (clostrum:find-class client environment name)))
      (apply #'make-instance class initargs)))

  (defmethod my-make-instance ((class class) &rest initargs)
    (apply #'make-instance class initargs))

  (setf (clostrum:fdefinition client environment 'make-instance)
        #'my-make-instance))

(defun define-ensure-class (client global-environment)
  (setf (clostrum:fdefinition client global-environment 'ensure-class)
        (lambda (name
                 &rest initargs
                 &key
                   direct-superclasses
                   direct-slots
                 &allow-other-keys)
          (apply #'closer-mop:ensure-class
                 (transform-name name)
                 :direct-superclasses (mapcar #'transform-name direct-superclasses)
                 :metaclass 'closer-mop:funcallable-standard-class
                 :direct-slots (mapcar #'transform-slot-spec direct-slots)
                 initargs)
          (setf (clo:find-class client global-environment name)
                (find-class (transform-name name))))))

(defun boot ()
  (let* ((client (make-instance 'client))
         (environment (create-environment))
         (global-environment (trucler:global-environment client environment))
         (*packages* (make-hash-table :test #'equal))
         (*symbol-package* (make-hash-table :test #'eq)))
    (reinitialize-instance client
      :environment global-environment)
    (import-from-host client global-environment)
    (import-khazern client global-environment)
    (define-environment-functions client global-environment)
    (define-package-functions client global-environment)
    (clostrum:make-variable
     client global-environment '*package* (find-package '#:common-lisp-user))
    (create-common-lisp-package client)
    (loop for name in '("COMMON-LISP-USER" "KEYWORD")
          do (setf (gethash name *packages*) (find-package name)))
    (define-make-instance client global-environment)
    (setf (clostrum:find-class client global-environment 'package)
          (find-class 'parcl-class:package))
    (define-ensure-class client global-environment)
    (sicl-new-boot:ensure-asdf-system
     client environment "sicl-clos-package")
    (sicl-new-boot:ensure-asdf-system
     client environment "sicl-clos-load-time")
    (values global-environment *packages* *symbol-package*)))
