(cl:in-package #:sicl-minimal-extrinsic-environment)

(defclass environment (sicl-simple-environment:simple-environment)
  ((%traced-functions :initform (make-hash-table :test #'eq)
                      :reader traced-functions)))

(defun trace (function-names environment)
  (loop for function-name in function-names
        do (if (sicl-genv:fboundp function-name environment)
               (setf (gethash (sicl-genv:fdefinition function-name environment)
                              (traced-functions environment))
                     t)
               (format *trace-output*
                       "Warning, function ~s not defined.  Not tracing~%"
                       function-name))))

(defun untrace (function-names environment)
  (loop for function-name in function-names
        do (when (sicl-genv:fboundp function-name environment)
             (remhash (sicl-genv:fdefinition function-name environment)
                      (traced-functions environment)))))
