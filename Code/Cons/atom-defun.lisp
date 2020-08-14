(cl:in-package #:sicl-cons)

(defun atom (object)
  (not (consp object)))
