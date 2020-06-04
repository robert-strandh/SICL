(cl:in-package #:sicl-boot)

(defparameter *table* (make-hash-table :test #'eq))

(defun my-untrace (&optional name)
  (if (null name)
      (maphash (lambda (key value)
                 (declare (ignore value))
                 (my-untrace key))
               *table*)
      (let ((original-function (gethash name *table*)))
        (unless (null original-function)
          (setf (sicl-genv:fdefinition name *e5*)
                original-function))
        (remhash name *table*))))

(defun my-trace (&rest names)
  (loop for name in names
        do (cond ((nth-value 1 (gethash name *table*))
                  (format *trace-output*
                          "~s is already traced.  Not tracding~%"
                          name))
                 ((not (sicl-genv:fboundp name *e5*))
                  (format *trace-output*
                          "The name ~s does not have a function associated with it.~%"
                          name))
                 (t (let ((original-function (sicl-genv:fdefinition name *e5*))
                          (name name))
                      (setf (gethash name *table*)
                            original-function)
                      (setf (sicl-genv:fdefinition name *e5*)
                            (lambda (&rest arguments)
                              (format *trace-output*
                                      "Calling ~s with arguments: ~s~%"
                                      name arguments)
                              (let ((values (multiple-value-list (apply original-function arguments))))
                                (format *trace-output*
                                        "Call to ~s returned values: ~s~%"
                                        name values)
                                (apply #'values values)))))))))
