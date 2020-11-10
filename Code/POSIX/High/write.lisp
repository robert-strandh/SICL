(cl:in-package #:sicl-posix-high)

(defun write (file-descriptor vector &key (start 0) (end nil))
  (let ((end (if (null end) (length vector) end)))
    (multiple-value-bind (effective-count error)
        (low:write file-descriptor vector start end)
      ;; for now, don't handle errors
      (assert (null error))
      effective-count)))

