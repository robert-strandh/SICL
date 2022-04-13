(cl:in-package #:sicl-boot-condition-system)

(defclass environment (sicl-boot:environment)
  ((%base :initarg :base :reader base)))

(defclass client (sicl-boot:client)
  ())

(defmethod env:fboundp ((client client) (environment environment) function-name)
  (or (call-next-method)
      (env:fboundp client (base environment) function-name)))

(defmethod trucler:describe-function
    ((client client) (environment environment) function-name)
  (let ((result (call-next-method)))
    (if (null result)
        (let* ((base (base environment))
               (base-client (env:client base)))
          (trucler:describe-function base-client base function-name))
        result)))

(defmethod trucler:describe-variable
    ((client client) (environment environment) name)
  (let ((result (call-next-method)))
    (if (null result)
        (let* ((base (base environment))
               (base-client (env:client base)))
          (trucler:describe-variable base-client base name))
        result)))

(defmethod env:fdefinition
    ((client client) (environment environment) function-name)
  (let ((result (call-next-method)))
    (if (null result)
        (let* ((base (base environment))
               (base-client (env:client base)))
          (env:fdefinition base-client base function-name))
        result)))

(defmethod env:macro-function
    ((client client) (environment environment) function-name)
  (let ((result (call-next-method)))
    (if (null result)
        (let* ((base (base environment))
               (base-client (env:client base)))
          (env:macro-function base-client base function-name))
        result)))

(defmethod env:function-cell
    ((client client) (environment environment) function-name)
  (let* ((base (base environment))
         (base-client (env:client base)))
    (if (null (env:fdefinition base-client environment function-name))
        (env:function-cell base-client base function-name)
        (call-next-method))))

(defmethod (setf env:fdefinition)
    (new-value (client client) (environment environment) function-name)
  (let* ((base (base environment))
         (base-client (env:client base)))
    (setf (env:fdefinition base-client base function-name)
          new-value)))
