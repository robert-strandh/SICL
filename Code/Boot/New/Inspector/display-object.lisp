(cl:in-package #:sicl-boot-inspector)

(defun display-object (frame pane)
  (let* ((stack (object-stack frame))
         (object (first stack)))
    (format pane "~a" (short-description object))))
