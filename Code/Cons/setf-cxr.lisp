(cl:in-package #:sicl-cons)

(defun (setf car) (new-value cons)
  (rplaca cons new-value)
  new-value)

(defun (setf cdr) (new-value cons)
  (rplacd cons new-value)
  new-value)

(defun (setf caar) (new-value cons)
  (setf (car (car cons)) new-value))

(defun (setf cadr) (new-value cons)
  (setf (car (cdr cons)) new-value))

(defun (setf (cdar) new-value cons)
  (setf (cdr (car cons)) new-value))
