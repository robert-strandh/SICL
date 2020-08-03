(cl:in-package #:sicl-boot-phase-8)

(defun find-class-name (class environment)
  (do-all-symbols (symbol nil)
    (when (eq (sicl-genv:find-class symbol environment) class)
      (return-from find-class-name symbol))))

(defun check-effective-slot-definition (effective-slot-definition environment)
  (if (not (typep effective-slot-definition 'sicl-boot::header))
      (format *trace-output* "    Effective slot definition is not a SICL object.~%")
      (progn 
        (let ((class (slot-value effective-slot-definition 'sicl-boot::%class)))
          (if (not (typep class 'sicl-boot::header))
              (format *trace-output* "    Class of effective slot definition is not a SICL object.~%")
              (let ((class-name (find-class-name class environment)))
                (when (null class-name)
                  (format *trace-output* "   Class of effective slot definition is not in environment.~%"))))))))

(defun check-effective-slot-definitions (effective-slot-definitions environment)
  (loop for effective-slot-definition in effective-slot-definitions
        do (check-effective-slot-definition effective-slot-definition environment)))

(defun check-instance-effective-slot-definitions (instance environment)
  (let ((rack (slot-value instance 'sicl-boot::%rack)))
    (check-effective-slot-definitions (aref rack 1) environment)))

(defun check-class-effective-slot-definitions (class environment)
  (let* ((fun (sicl-genv:fdefinition 'sicl-clos:class-slots environment))
         (effective-slot-definitions (funcall fun class)))
    (check-effective-slot-definitions effective-slot-definitions environment)))

(defun check-direct-slot-definition (direct-slot-definition environment)
  (if (not (typep direct-slot-definition 'sicl-boot::header))
      (format *trace-output* "    Direct slot definition is not a SICL object.~%")
      (progn 
        (let ((class (slot-value direct-slot-definition 'sicl-boot::%class)))
          (if (not (typep class 'sicl-boot::header))
              (format *trace-output* "    Class of direct slot definition is not a SICL object.~%")
              (let ((class-name (find-class-name class environment)))
                (when (null class-name)
                  (format *trace-output* "   Class of direct slot definition is not in environment.~%"))))))))

(defun check-direct-slot-definitions (class environment)
  (let* ((fun (sicl-genv:fdefinition 'sicl-clos:class-direct-slots environment))
         (direct-slot-definitions (funcall fun class)))
    (loop for direct-slot-definition in direct-slot-definitions
          do (check-direct-slot-definition direct-slot-definition environment))))

(defun check-metaclass (class environment)
  (let ((metaclass (slot-value class 'sicl-boot::%class)))
    (if (not (typep metaclass 'sicl-boot::header))
        (format *trace-output* "    Metaclass is not a SICL object.~%")
        (let ((metaclass-name (find-class-name metaclass environment)))
          (when (null metaclass-name)
            (format *trace-output* "   Metaclass is not a class in environment.~%"))))))

(defun check-superclass (superclass environment)
  (let ((superclass-name (find-class-name superclass environment)))
    (when (null superclass-name)
      (format *trace-output* "   Superclass is not a class in environment.~%"))))

(defun check-superclasses (class environment)
  (let* ((fun (sicl-genv:fdefinition 'sicl-clos:class-direct-superclasses environment))
         (superclasses (funcall fun class)))
    (loop for superclass in superclasses
          do (check-superclass superclass environment))))

(defun check-subclass (subclass environment)
  (let ((subclass-name (find-class-name subclass environment)))
    (when (null subclass-name)
      (format *trace-output* "   Subclass is not a class in environment.~%"))))

(defun check-subclasses (class environment)
  (let* ((fun (sicl-genv:fdefinition 'sicl-clos:class-direct-subclasses environment))
         (subclasses (funcall fun class)))
    (loop for subclass in subclasses
          do (check-subclass subclass environment))))

(defun check-class (name class environment)
  (format *trace-output* "Checking class named ~s~%" name)
  (if (not (typep class 'sicl-boot::header))
      (format *trace-output* "    Class named ~s is not a SICL object.~%" name)
      (progn (check-metaclass class environment)
             (check-instance-effective-slot-definitions class environment)
             (check-class-effective-slot-definitions class environment)
             (check-direct-slot-definitions class environment)
             (check-superclasses class environment)
             (check-subclasses class environment))))

(defun check-classes (environment)
  (let ((table (make-hash-table :test #'eq)))
    (do-all-symbols (symbol)
      (unless (gethash symbol table)
        (setf (gethash symbol table) t)
        (let ((potential-class (sicl-genv:find-class symbol environment)))
          (unless (null potential-class)
            (check-class symbol potential-class environment)))))))
