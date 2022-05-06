(cl:in-package #:sicl-boot-alexandria)

(defclass client (sicl-boot:client) ())

(defparameter *host-function-names*
  '(format
    cleavir-code-utilities:parse-destructuring-bind
    fourth))

(defun boot (boot)
  (with-accessors ((e5 sicl-boot:e5))
      boot
    (let* ((client (make-instance 'client))
           (sicl-client:*client* client))
      (let (functions)
        (unwind-protect
             (progn (setf functions
                          (loop for name in *host-function-names*
                                collect (env:fdefinition client e5 name)))
                    (import-functions-from-host *host-function-names* e5)
                    (ensure-asdf-system-using-client client e5 '#:alexandria))
          (loop for name in *host-function-names*
                for function in functions
                do (setf (env:fdefinition client e5 name) function)))))))

