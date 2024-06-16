(cl:in-package #:sicl-new-boot)

(defun bt ()
  (sicl-new-boot-backtrace-inspector:inspect
   common-boot-ast-evaluator::*stack*))
