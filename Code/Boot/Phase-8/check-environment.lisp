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

(defun check-direct-default-initarg (direct-default-initarg environment)
  (let ((thunk (third direct-default-initarg)))
    (if (not (typep thunk 'sicl-boot::header))
        (format *trace-output* "    Thunk of direct default initarg is not a SICL object.~%")
        (let* ((thunk-class (slot-value thunk 'sicl-boot::%class))
               (class-name (find-class-name thunk-class environment)))
          (when (null class-name)
            (format *trace-output* "   Class of direct defualt initarg thunk is not in environment.~%"))))))

(defun check-direct-default-initargs (class environment)
  (let* ((fun (sicl-genv:fdefinition 'sicl-clos:class-direct-default-initargs environment))
         (direct-default-initargs (funcall fun class)))
    (loop for direct-default-initarg in direct-default-initargs
          do (check-direct-default-initarg direct-default-initarg environment))))

(defun check-precedence-list (class environment)
  (let* ((fun (sicl-genv:fdefinition 'sicl-clos:class-precedence-list environment))
         (precedence-list (funcall fun class)))
    (loop for superclass in precedence-list
          do (check-superclass superclass environment))))

(defun check-class (name class environment)
  (format *trace-output* "Checking class named ~s~%" name)
  (if (not (typep class 'sicl-boot::header))
      (format *trace-output* "    Class named ~s is not a SICL object.~%" name)
      (progn (check-metaclass class environment)
             (check-instance-effective-slot-definitions class environment)
             (check-class-effective-slot-definitions class environment)
             (check-direct-slot-definitions class environment)
             (check-superclasses class environment)
             (check-subclasses class environment)
             (check-direct-default-initargs class environment)
             (check-precedence-list class environment))))

(defun check-classes (environment)
  (let ((table (make-hash-table :test #'eq)))
    (do-all-symbols (symbol)
      (unless (gethash symbol table)
        (setf (gethash symbol table) t)
        (let ((potential-class (sicl-genv:find-class symbol environment)))
          (unless (null potential-class)
            (check-class symbol potential-class environment)))))))

(defun check-function-class (function environment)
  (let ((class (slot-value function 'sicl-boot::%class)))
    (if (not (typep class 'sicl-boot::header))
        (format *trace-output* "    Function class is not a SICL object.~%")
        (let ((class-name (find-class-name class environment)))
          (when (null class-name)
            (format *trace-output* "   Function class is not a class in environment.~%"))))))

(defun object-is-function-cell-in-environment-p (element environment)
  (maphash (lambda (function-name function-entry)
             (declare (ignore function-name))
             (when (eq (sicl-simple-environment::function-cell function-entry)
                       element)
               (return-from object-is-function-cell-in-environment-p t)))
           (sicl-simple-environment::function-entries environment))
  nil)

(defun check-static-environment-element (index element environment)
  (cond ((object-is-function-cell-in-environment-p element environment)
         nil)
        ((consp element)
         ;; We hope that we don't have any circular lists in the
         ;; static environment.
         (check-static-environment-element index (car element) environment)
         (check-static-environment-element index (cdr element) environment))
        ((typep element 'sicl-boot::header)
         (let* ((class (slot-value element 'sicl-boot::%class))
                (name (find-class-name class environment)))
           (when (null name)
             (format *trace-output*
                     "    Index ~s of static environment contains an impure object.~%"
                     index))))
        ((<= 1 index 2)
         ;; These elements are always the ENCLOSE and the
         ;; INITIALIZE-CLOSURE functions.
         nil)
        ((typep element '(or integer symbol string float))
         nil)
        ((member element (list #'cons #'apply))
         nil)
        (t
         (format *trace-output*
                 "    Index ~s of static environment contains an invalid object.~%"
                 index))))

(defun check-static-environment (function environment)
  (let* ((fname 'sicl-clos::environment)
         (static-environment-function (sicl-genv:fdefinition fname environment))
         (env (funcall static-environment-function function)))
    (cond ((null env)
           nil)
          ((typep env 'vector)
           (loop for element across env
                 for i from 0
                 do (check-static-environment-element i element environment)))
          (t
           (format *trace-output*
                   "    ~s is not a valid value for the static environment.~%"
                   env)))))

(defun check-function (name function environment)
  (format *trace-output* "Checking function named ~s~%" name)
  (if (not (typep function 'sicl-boot::header))
      (format *trace-output* "    Function is not a SICL object.~%")
      (progn (check-function-class function environment)
             (check-static-environment function environment))))

(defun check-functions (environment)
  (let ((table (make-hash-table :test #'eq)))
    (do-all-symbols (symbol)
      (unless (gethash symbol table)
        (setf (gethash symbol table) t)
        (when (and (sicl-genv:fboundp symbol environment)
                   (not (sicl-genv:special-operator symbol environment))
                   (null (sicl-genv:macro-function symbol environment)))
          (let ((function (sicl-genv:fdefinition symbol environment)))
            (check-function symbol function environment)))
        (when (sicl-genv:fboundp `(setf ,symbol) environment)
          (let ((function (sicl-genv:fdefinition `(setf ,symbol) environment)))
            (check-function `(setf ,symbol) function environment)))))))
