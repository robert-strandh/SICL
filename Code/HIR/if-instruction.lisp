(cl:in-package #:sicl-hir)

;;; This instruction has one input, no outputs, and two successors.
;;; If the input is true, then the first successor is chosen,
;;; otherwise the second successor is chosen.

(defclass if-instruction (instruction)
  ())
