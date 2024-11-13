(cl:in-package #:sicl-new-boot)

(defclass trace-wrapper (closer-mop:funcallable-standard-object)
  ((%original-function
    :initarg :original-function
    :reader original-function))
  (:metaclass closer-mop:funcallable-standard-class))

(defparameter *indentation* 0)

(defun strace (name environment)
  (let* ((client (make-instance 'client))
         (original-function
           (clostrum:fdefinition client environment name))
         (wrapper
           (make-instance 'trace-wrapper
             :original-function original-function)))
    (closer-mop:set-funcallable-instance-function
     wrapper
     (lambda (&rest arguments)
       (loop repeat *indentation*
             do (format *trace-output* "  "))
       (format *trace-output*
               "Calling ~s with arguments ~s in ~s~%"
               name arguments environment)
       (let ((values
               (let ((*indentation* (1+ *indentation*)))
                 (multiple-value-list (apply original-function arguments)))))
         (loop repeat *indentation*
               do (format *trace-output* "  "))
         (format *trace-output*
                 "~s in ~s returned values ~s~%"
                 name environment values)
         (apply #'values values))))
    (setf (clostrum:fdefinition client environment name)
          wrapper)))

(defun suntrace (name environment)
  (let* ((client (make-instance 'client))
         (wrapped-function
           (clostrum:fdefinition client environment name)))
    (when (typep wrapped-function 'trace-wrapper)
      (setf (clostrum:fdefinition client environment name)
            (original-function wrapped-function)))))
