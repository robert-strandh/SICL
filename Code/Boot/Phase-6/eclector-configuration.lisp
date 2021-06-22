(cl:in-package #:sicl-boot-phase-6)

(defparameter *symbol-names* nil)

(defmethod eclector.reader:interpret-symbol :around 
    ((client sicl-boot-phase-5::client) input-stream package-indicator symbol-name internp)
  (let* ((result (call-next-method)))
    (when (null (gethash result *symbol-names*))
      (let* ((make-string-function
               (env:fdefinition client (sicl-boot:environment client) 'make-string))
             (setf-aref-function
               (env:fdefinition client (sicl-boot:environment client) '(setf aref)))
             (string (funcall make-string-function (length symbol-name))))
        (loop for i from 0 below (length symbol-name)
              do (funcall setf-aref-function (aref symbol-name i) string i))
        (setf (gethash result *symbol-names*) string)))
    result))
