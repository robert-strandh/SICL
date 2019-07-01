(cl:in-package #:sicl-boot-inspector)

(defgeneric display-object (object frame pane))

(defun display-stack-top (frame pane)
  (let* ((stack (object-stack frame))
         (object (first stack)))
    (display-object object frame pane)))

(defmethod display-object (object frame pane)
  (format pane "~a" (short-description object)))

(defmethod display-object ((object number) frame pane)
  (format pane "A host number~%~s~%" object))

(defmethod display-object ((object character) frame pane)
  (format pane "A host character~%~s~%" object))

(defmethod display-object ((object string) frame pane)
  (format pane "A host string~%~s~%" object))

(defmethod display-object ((object symbol) frame pane)
  (format pane "A host symbol~%~s~%" object))

(defmethod display-object ((object package) frame pane)
  (format pane "A host package~%~s~%" object))
