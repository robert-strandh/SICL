(cl:in-package #:sicl-environment)

(defun boundp (name)
  (let ((cell (clostrum-sys:variable-cell *client* *environment* name)))
    (when (null cell)
      (setf cell (cons nil nil)))
    (rt:boundp name cell rt:*dynamic-environment*)))
