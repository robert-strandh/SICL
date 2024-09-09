(cl:in-package #:sicl-new-boot)

(defun bt ()
  (sicl-new-boot-backtrace-inspector:inspect cbae::*stack*))
