(cl:in-package #:sicl-boot)

(defun finalize-all-classes (ea)
  (format *trace-output* "Finalizing all classes.~%")
  (let* ((finalized-p-function
           (sicl-genv:fdefinition 'sicl-clos:class-finalized-p ea))
         (finalization-function
           (sicl-genv:fdefinition 'sicl-clos:finalize-inheritance ea)))
    (do-all-symbols (var)
      (let ((class (sicl-genv:find-class var ea)))
        (unless (or (null class)
                    (funcall finalized-p-function class))
          (funcall finalization-function class)))))
  (format *trace-output* "Done finalizing all classes.~%"))
