(cl:in-package #:sicl-symbol)

(let ((variable-cell-function #'variable-cell))
  (defun symbol-value (symbol)
    (let ((cell (funcall variable-cell-function symbol)))
      (sicl-run-time:symbol-value symbol cell)))

  (defun (setf symbol-value) (new-value symbol)
    (let ((cell (funcall variable-cell-function symbol)))
      (setf (sicl-run-time:symbol-value symbol cell)
            new-value)))

  (defun boundp (symbol)
    (let ((cell (funcall variable-cell-function symbol)))
      (sicl-run-time:boundp symbol cell)))

  (defun makunbound (symbol)
    (let ((cell (funcall variable-cell-function symbol)))
      (sicl-run-time:makunbound symbol cell))))
