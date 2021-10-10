(cl:in-package #:sicl-hir-to-mir)

;;; Avoid passing the code object as an argument to every call to
;;; PROCESS-INSTRUCTION, since only a few methods actually use it.
(defvar *code-object*)
