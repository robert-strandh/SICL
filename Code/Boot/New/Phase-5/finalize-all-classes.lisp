(cl:in-package #:sicl-boot-phase-5)

(defun finalize-all-classes (boot)
  (format *trace-output* "Finalizing all classes.~%")
  (let* ((e4 (sicl-boot:e4 boot))
         (finalization-function
           (sicl-genv:fdefinition 'sicl-clos:finalize-inheritance e4)))
    (import-function-from-host 'not e4)
    (do-all-symbols (var)
      (let ((class (sicl-genv:find-class var e4)))
        (unless (null class)
          (funcall finalization-function class)))))
  (format *trace-output* "Done finalizing all classes.~%"))
