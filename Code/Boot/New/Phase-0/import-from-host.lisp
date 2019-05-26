(cl:in-package #:sicl-boot-phase-0)

(defun import-from-host (environment)
  ;; Import every function from the COMMON-LISP package.
  (do-symbols (symbol (find-package '#:common-lisp))
    (when (fboundp symbol)
      (let ((definition (fdefinition symbol)))
        (when (functionp definition)
          (setf (sicl-genv:fdefinition symbol environment) definition))))
    (when (fboundp `(setf ,symbol))
      (setf (sicl-genv:fdefinition symbol environment)
            (fdefinition `(setf ,symbol))))))
