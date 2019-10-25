(cl:in-package #:sicl-character)

(defun char-upcase (character)
  (cl-unicode:uppercase-mapping character))
