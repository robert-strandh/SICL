(cl:in-package #:cleavir-type-inference)

(defvar *dictionary*)

(defun arc-bag (from to dictionary)
  (gethash (cons from to) dictionary))

(defvar *work-list*)
