(cl:in-package #:sicl-boot-fill-target-environment)

(defclass client (sicl-client:sicl)
  (;; This slot contains a target environment represented as an ersatz
   ;; object.
   (%target-environment
    :initarg :target-environment
    :reader target-environment)
   ;; This slot contains the target function ENV:FDEFINITION, We can
   ;; pass the target environment in the previous slot to this
   ;; function and get access to any function in the target
   ;; environment.
   (%target-fdefinition
    :initarg :target-fdefinition
    :reader target-fdefinition)))

(defmethod env:fdefinition
    ((client client) (environment sicl-boot:header) function-name)
  (funcall (target-fdefinition client)
           client environment function-name))

(defmethod (setf env:fdefinition)
    (new-definition (client client) (environment sicl-boot:header) function-name)
  (let ((target-setf-fdefinition
          (funcall (target-fdefinition client)
                   client environment '(setf fdefinition))))
    (funcall target-setf-fdefinition
             new-definition client environment function-name)))

(defmethod env:find-class
    ((client client) (environment sicl-boot:header) class-name)
  (let ((target-find-class
          (funcall (target-fdefinition client)
                   client environment 'find-class)))
    (funcall target-find-class
             client environment class-name)))

(defmethod (setf env:find-class)
    (new-class (client client) (environment sicl-boot:header) class-name)
  (let ((target-setf-find-class
          (funcall (target-fdefinition client)
                   client environment '(setf find-class))))
    (funcall target-setf-find-class
             new-class client environment class-name)))

(defmethod env:constant-variable
    ((client client) (environment sicl-boot:header) symbol)
  (let ((target-constant-variable
          (funcall (target-fdefinition client)
                   client environment 'constant-variable)))
    (funcall target-constant-variable
             client environment symbol)))

(defmethod (setf env:constant-variable)
    (new-class (client client) (environment sicl-boot:header) symbol)
  (let ((target-setf-constant-variable
          (funcall (target-fdefinition client)
                   client environment '(setf constant-variable))))
    (funcall target-setf-constant-variable
             new-class client environment symbol)))

(defmethod env:special-variable
    ((client client) (environment sicl-boot:header) symbol)
  (let ((target-special-variable
          (funcall (target-fdefinition client)
                   client environment 'special-variable)))
    (funcall target-special-variable
             client environment symbol)))
