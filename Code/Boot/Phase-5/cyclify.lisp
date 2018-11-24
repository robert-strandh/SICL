(cl:in-package #:sicl-boot-phase-5)

(defun patch-class-slot (instance boot)
  (with-accessors ((e2 sicl-boot:e2)
		   (e3 sicl-boot:e3))
      boot
    (with-slots ((class sicl-boot-phase-2::%class))
	instance
      (unless (typep class 'sicl-boot-phase-2::header)
        (let* ((class-name-fun (sicl-genv:fdefinition 'class-name e2))
               (class-name (funcall class-name-fun class))
               (new-class (sicl-genv:find-class class-name e3)))
          (setf class new-class))))))

(defun patch-slot-definition (slot-definition boot)
  (patch-class-slot slot-definition boot))

(defun patch-class (class boot)
  (with-accessors ((e2 sicl-boot:e2)
		   (e3 sicl-boot:e3)
                   (e4 sicl-boot:e4))
      boot
    (let* ((direct-slots-function
             (sicl-genv:fdefinition 'sicl-clos:class-direct-slots e3))
	   (direct-slots (funcall direct-slots-function class))
	   (effective-slots-function
             (sicl-genv:fdefinition 'sicl-clos:class-slots e3))
	   (effective-slots (funcall effective-slots-function class)))
      (loop for slot-definition in direct-slots
	    do (patch-slot-definition slot-definition boot))
      (loop for slot-definition in effective-slots
	    do (patch-slot-definition slot-definition boot)))
    (patch-class-slot class boot)))

(defun patch-method-combination (method-combination boot)
  (patch-class-slot method-combination boot))

(defun patch-method-combination-template (method-combination-template boot)
  (patch-class-slot method-combination-template boot))

(defun patch-method (method boot)
  (patch-class-slot method boot))

(defun patch-generic-function (generic-function boot)
  (with-accessors ((e2 sicl-boot:e2)
		   (e3 sicl-boot:e3)
                   (e4 sicl-boot:e4))
      boot
    (let ((shared-initialize (sicl-genv:fdefinition 'shared-initialize e2))
	  (method-class (sicl-genv:find-class 'standard-method e4)))
      (funcall shared-initialize
	       generic-function
	       '()
	       :method-class method-class))
    (let* ((methods-function
             (sicl-genv:fdefinition 'sicl-clos:generic-function-methods e3))
	   (methods (funcall methods-function generic-function)))
      (loop for method in methods
	    do (patch-method method boot))))
  (patch-class-slot generic-function boot))

(defun cyclify (boot)
  (with-accessors ((e2 sicl-boot:e2)
		   (e3 sicl-boot:e3)
                   (e4 sicl-boot:e4))
      boot
    (do-all-symbols (symbol)
      (when (sicl-genv:fboundp symbol e4)
	(let ((function (sicl-genv:fdefinition symbol e4)))
	  (when (typep function 'sicl-boot-phase-2::header)
	    (patch-generic-function function boot))))
      (when (sicl-genv:fboundp `(setf ,symbol) e4)
	(let ((function (sicl-genv:fdefinition `(setf ,symbol) e4)))
	  (when (typep function 'sicl-boot-phase-2::header)
	    (patch-generic-function function boot))))
      (let ((class-or-nil (sicl-genv:find-class symbol e3)))
	(unless (null class-or-nil)
	  (patch-class class-or-nil boot))))))
