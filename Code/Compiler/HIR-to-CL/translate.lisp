(cl:in-package #:sicl-hir-to-cl)

(defvar *visited*)

(defgeneric translate (instruction))
