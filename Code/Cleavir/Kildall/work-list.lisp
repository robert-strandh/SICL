(in-package #:cleavir-kildall)

(defvar *work-list*)

(declaim (inline add-work pop-work))

(defun add-work (instruction) (push instruction *work-list*))
(defun pop-work () (pop *work-list*))
