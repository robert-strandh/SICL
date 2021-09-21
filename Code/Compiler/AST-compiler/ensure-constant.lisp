(cl:in-package #:sicl-compiler)

(defun ensure-constant (constants constant)
  (let ((pos (position constant constants)))
    (if (null pos)
        (vector-push-extend constant constants)
        pos)))
