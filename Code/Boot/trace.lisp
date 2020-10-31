(cl:in-package #:sicl-boot)

(defun call-with-etrace (environment function-names)
  (loop with traced-functions = (env:traced-functions environment)
        with env = environment
        for name in function-names
        for function = (env:fdefinition (env:client env) env name)
        do (setf (gethash name traced-functions) function)
           (setf (env:fdefinition (env:client env) env name)
                 (let ((function function)
                       (name name))
                   (lambda (&rest arguments)
                     (format *trace-output*
                             "Calling ~s in environment ~s with arguments ~s~%"
                             name env arguments)
                     (let ((values (multiple-value-list (apply function arguments))))
                       (format *trace-output*
                               "Call to ~s in environment ~s returned values ~s~%"
                               name env values)
                       (apply #'values values)))))))

(defun call-with-euntrace (environment function-names)
  (let ((traced-functions (env:traced-functions environment))
        (env environment))
    (if (null function-names)
        (loop for name being each hash-key of traced-functions
                using (hash-value function)
              do (setf (env:fdefinition (env:client env) env name)
                       function)
              finally (clrhash traced-functions))
        (loop for name in function-names
              do (setf (env:fdefinition (env:client env) env name)
                       (gethash name traced-functions))
                 (remhash name traced-functions)))))

(defmacro etrace (environment-form &rest function-names)
  `(call-with-etrace ,environment-form ',function-names))

(defmacro euntrace (environment-form &rest function-names)
  `(call-with-euntrace ,environment-form ',function-names))
