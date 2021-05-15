(cl:in-package #:sicl-boot)

;;; An instance of this class is used both as the CLIENT argument in
;;; operations that require such an argument, and as an ASDF operation
;;; object to pass to ASDF:OPERATE.
(defclass client
    (sicl-client:sicl asdf/action:downward-operation asdf/action:selfward-operation)
  ((%environment :initarg :environment :reader environment)
   (asdf:selfward-operation :initform '(prepare-op) :allocation :class)))

(defclass prepare-op (asdf/action:upward-operation asdf/action:sideway-operation)
  ((asdf:sideway-operation :initform 'client :allocation :class)))

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

(defmethod asdf:perform
    ((operation client) (component asdf/lisp-action:cl-source-file))
  (format *trace-output* "Loading ~s~%" component)
  (load-source-file-common
   (asdf/component:component-pathname component)
   (environment operation)))

(defmethod asdf:perform ((operation client) (component asdf/system:system))
  (format *trace-output*
          "Done loading system ~s~%"
          component))

(defmethod asdf:perform ((operation prepare-op) component)
  (format *trace-output*
          "Not preparing component ~s~%"
          component))

(defmethod asdf:perform ((operation prepare-op) (component asdf/system:system))
  (format *trace-output*
          "Loading system ~s~%"
          component))

(defmethod asdf/action:compute-action-stamp
    (plan (operation prepare-op) component &key just-done)
  (if just-done
      (values (get-universal-time) t)
      (values nil nil)))

(defmethod asdf/action:compute-action-stamp
    (plan (operation client) component &key just-done)
  (if just-done
      (values (get-universal-time) t)
      (values nil nil)))

(defmethod asdf/action:compute-action-stamp
    (plan (operation client) (component asdf/lisp-action:cl-source-file) &key just-done)
  (if just-done
      (values (get-universal-time) t)
      (let* ((files (loaded-files (environment operation)))
             (path (asdf/component:component-pathname component))
             (entry (assoc path files :test #'equal)))
        (if (null entry)
            (values nil nil)
            (values (cdr entry) t)))))
