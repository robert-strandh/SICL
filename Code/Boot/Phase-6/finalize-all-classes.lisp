(cl:in-package #:sicl-boot-phase-6)

(defun finalize-all-classes (boot)
  (format *trace-output* "Finalizing all classes.~%")
  (let* ((e5 (sicl-boot:e5 boot))
         (finalization-function
           (sicl-genv:fdefinition 'sicl-clos:finalize-inheritance e5)))
    (do-all-symbols (var)
      (let ((class (sicl-genv:find-class var e5)))
        (unless (null class)
          (funcall finalization-function class)))))
  (format *trace-output* "Done finalizing all classes.~%"))
