(cl:in-package #:sicl-boot)

;;; An instance of this class is used both as the CLIENT argument in
;;; operations that require such an argument, and as an ASDF operation
;;; object to pass to ASDF:OPERATE.
(defclass client (sicl-client:sicl asdf:operation)
  ((%environment :initarg :environment :reader environment)
   (asdf:sideway-operation :initform 'client :allocation :class)))

(defun make-client (class &rest initargs)
  (let ((client (asdf:make-operation class)))
    (apply #'reinitialize-instance client initargs)
    client))

(defgeneric loaded-files (environment))

(defgeneric (setf loaded-files) (loaded-files environment))

(defmethod asdf:perform ((operation client) component)
  (format *trace-output*
          "Not acting upon component ~s~%"
          component))

(defmethod asdf:perform ((operation client) (component asdf/system:system))
  (format *trace-output*
          "Done loading system ~s~%"
          component))
