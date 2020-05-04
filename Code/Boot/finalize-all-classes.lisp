(cl:in-package #:sicl-boot)

(defun finalize-all-classes (ea)
  (format *trace-output* "Finalizing all classes.~%")
  (let* ((finalization-function
           (sicl-genv:fdefinition 'sicl-clos:finalize-inheritance ea)))
    (do-all-symbols (var)
      (let ((class (sicl-genv:find-class var ea)))
        (unless (null class)
          (funcall finalization-function class)))))
  (format *trace-output* "Done finalizing all classes.~%"))
