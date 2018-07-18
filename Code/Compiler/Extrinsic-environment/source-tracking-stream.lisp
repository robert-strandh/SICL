(cl:in-package #:sicl-extrinsic-environment)

;;; Turn the contents of STREAM into a vector of lines, where each
;;; line is a string.
(defun read-entire-stream (stream)
  (let ((lines (loop for line = (read-line stream nil nil)
                     until (null line)
                     collect line)))
    (make-array (length lines) :initial-contents lines)))
