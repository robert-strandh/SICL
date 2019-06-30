(cl:in-package #:sicl-boot-inspector)

(defgeneric display-object (object frame pane))

(defun display-stack-top (frame pane)
  (let* ((stack (object-stack frame))
         (object (first stack)))
    (display-object object frame pane)))

(defmethod display-object (object frame pane)
  (format pane "~a" (short-description object)))
