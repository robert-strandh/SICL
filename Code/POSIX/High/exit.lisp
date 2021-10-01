(cl:in-package #:sicl-posix-high)

(defun exit (status)
  (sicl-posix-low:exit status))
