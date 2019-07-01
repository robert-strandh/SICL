(cl:in-package #:sicl-boot-inspector)

(defclass inspectable-object () ())

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

(defun display-element (element index frame pane)
  (declare (ignore frame))
  (format pane "~s: " index)
  (clim:with-output-as-presentation (pane element 'inspectable-object)
    (format pane "~s" (short-description element)))
  (format pane "~%"))

(defun display-proper-list (object frame pane)
  (format pane "A host proper list~%")
  (loop for element in object
        for i from 0
        do (display-element element i frame pane)))

(defun display-circular-list (object frame pane)
  (format pane "A host circular list~%")
  (loop with table = (make-hash-table :test #'eq)
        for remaining = object then (cdr remaining)
        for i from 0
        until (gethash remaining table)
        do (setf (gethash remaining table) t)
           (display-element (car remaining) i frame pane)))

(defun display-dotted-list (object frame pane)
  (format pane "A host dotted list~%")
  (loop for remaining = object then (cdr remaining)
        for i from 0
        until (atom remaining)
        do (display-element (car remaining) i frame pane)
        finally
           (format pane " .~%")
           (display-element remaining (1+ i) frame pane)))

(defmethod display-object ((object cons) frame pane)
  (cond ((proper-list-p object)
         (display-proper-list object frame pane))
        ((circular-list-p object)
         (display-circular-list object frame pane))
        (t
         (display-dotted-list object frame pane))))
