(cl:in-package #:sicl-compiler)

(defun ensure-constant (code-object constant)
  (let* ((constants (constants code-object))
         (pos (position constant constants)))
    (if (null pos)
        (vector-push-extend constant constants)
        pos)))
