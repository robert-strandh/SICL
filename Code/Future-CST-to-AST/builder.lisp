(cl:in-package #:sicl-future-cst-to-ast)

(defgeneric environment (builder))

(defgeneric client (builder))

(defclass builder (bld:builder)
  ((%client
    :initarg :client
    :reader client)
   (%environment
    :initarg :environment
    :reader environment)))

(defun make-builder (client environment)
  (make-instance 'builder
    :client client
    :environment environment))

(defun builder-components (builder)
  (values (client builder) (environment builder)))

(defmacro with-builder-components
    ((builder-form client-name environment-name) &body body)
  `(multiple-value-bind (,client-name ,environment-name)
       ,builder-form
     ,@body))
