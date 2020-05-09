(cl:in-package #:sicl-boot-trace)

(defparameter *trace-depth* 0)

(defparameter *traced-functions*
  (make-hash-table :test #'equal))

(defun untrace (&optional function-name environment)
  (cond ((null function-name)
         (maphash (lambda (key value)
                    (declare (ignore value))
                    (destructuring-bind (name . environment)
                        key
                      (untrace name environment)))
                  *traced-functions*))
        ((null environment)
         (maphash (lambda (key value)
                    (declare (ignore value))
                    (destructuring-bind (name . environment)
                        key
                      (when (equal function-name name)
                        (untrace name environment))))
                  *traced-functions*))
        (t
         (setf (sicl-genv:fdefinition function-name environment)
               (gethash (cons function-name environment)
                        *traced-functions*))
         (remhash (cons function-name environment)
                  *traced-functions*))))

(defun indent ()
  (loop repeat *trace-depth*
        do (format *trace-output* "  ")))

(defun tracable-p (function-name environment)
  (and (sicl-genv:fboundp function-name environment)
       (not (consp (sicl-genv:fdefinition function-name environment)))))

(defun already-traced-p (function-name environment)
  (nth-value 1 (gethash (cons function-name environment) *traced-functions*)))

(defun print-value (value environment use-print-object-p)
  (cond ((typep value 'array)
         (format *trace-output* "[array]"))
        ((and use-print-object-p (typep value 'sicl-boot::header))
         (handler-case
             (funcall (sicl-genv:fdefinition 'print-object environment)
                      value
                      *trace-output*)
           (error ()
             (format *trace-output*
                     "[*]~s"
                     value))))
        (t
         (format *trace-output*
                 "~s"
                 value))))

(defun trace (function-name environment &key use-print-object-p)
  (cond ((not (tracable-p function-name environment))
         (format *trace-output*
                 "Function ~s is not fbound in environment ~s.  Not tracing!~%"
                 function-name
                 environment))
        ((already-traced-p function-name environment)
         (format *trace-output*
                 "Function ~s is already traced.  Not tracing!~%"
                 function-name))
        (t
         (let ((function (sicl-genv:fdefinition function-name environment)))
           (setf (gethash (cons function-name environment) *traced-functions*)
                 function)
           (setf (sicl-genv:fdefinition function-name environment)
                 (lambda (&rest arguments)
                   (when (> *trace-depth* 100)
                     (setf *trace-depth* 0)
                     (throw 'out nil))
                   (indent)
                   (format *trace-output*
                           "Function named ~s called in environment ~s~%"
                           function-name
                           environment)
                   (indent)
                   (format *trace-output* "With arguments:~%")
                   (incf *trace-depth* 4)
                   (loop for argument in arguments
                         do (indent)
                            (format *trace-output* "=>  ")
                            (print-value argument environment use-print-object-p)
                            (format *trace-output* "~%"))
                   (decf *trace-depth* 4)
                   (incf *trace-depth*)
                   (let ((values (multiple-value-list (apply function arguments))))
                     (decf *trace-depth*)
                     (indent)
                     (format *trace-output*
                             "~s returned: "
                             function-name)
                     (loop for value in values
                           do (print-value value environment use-print-object-p))
                     (format *trace-output* "~%")
                     (apply #'values values))))))))

(defun trace-all (environment &key use-print-object-p)
  (do-all-symbols (var)
    (unless (or (eq var 'print-object)
                (eq var 'format))
      (when (and (tracable-p var environment)
                 (not (already-traced-p var environment)))
        (trace var environment :use-print-object-p use-print-object-p))
      (when (and (tracable-p `(setf ,var) environment)
                 (not (already-traced-p `(setf ,var) environment)))
        (trace `(setf ,var) environment :use-print-object-p use-print-object-p)))))
