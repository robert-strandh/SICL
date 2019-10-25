(cl:in-package #:sicl-character)

(defun char-downcase (character)
  (cl-unicode:lowercase-mapping character))
