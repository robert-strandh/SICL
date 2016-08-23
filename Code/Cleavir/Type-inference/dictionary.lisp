(cl:in-package #:cleavir-type-inference)

(defvar *dictionary*)

(defun arc-bag (from to dictionary)
  (gethash (cons from to) dictionary))
(defun (setf arc-bag) (new-value from to dictionary)
  (setf (gethash (cons from to) dictionary) new-value))

(defvar *work-list*)
