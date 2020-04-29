(cl:in-package #:sicl-boot-phase-4)

(defun finalize-all-classes (boot)
  (format *trace-output* "Finalizing all classes.~%")
  (let* ((e3 (sicl-boot:e3 boot))
         (finalization-function
           (sicl-genv:fdefinition 'sicl-clos:finalize-inheritance e3)))
    (do-all-symbols (var)
      (let ((class (sicl-genv:find-class var e3)))
        (unless (null class)
          (funcall finalization-function class)))))
  (format *trace-output* "Done finalizing all classes.~%"))
