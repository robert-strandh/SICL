(cl:in-package #:sicl-register-allocation)

(defvar *input-arrangements*)

(defun input-arrangement-p (instruction)
  (nth-value 1 (gethash instruction *input-arrangements*)))

(defun input-arrangement (instruction)
  (multiple-value-bind (value value-p)
      (gethash instruction *input-arrangements*)
    (assert value-p)
    value))

(defun (setf input-arrangement) (arrangement instruction)
  (unless (null arrangement)
    (arr::check-arrangement-integrity arrangement)
    (setf (arr:frozen-p arrangement) t))
  (setf (gethash instruction *input-arrangements*) arrangement))

(defvar *output-arrangements*)

(defun output-arrangement (instruction)
  (multiple-value-bind (value value-p)
      (gethash instruction *output-arrangements*)
    (assert value-p)
    value))

(defun (setf output-arrangement) (arrangement instruction)
  (unless (null arrangement)
    (arr::check-arrangement-integrity arrangement)
    (setf (arr:frozen-p arrangement) t))
  (setf (gethash instruction *output-arrangements*) arrangement))
