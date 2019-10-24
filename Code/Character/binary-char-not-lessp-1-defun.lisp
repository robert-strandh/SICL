(cl:in-package #:sicl-character)

(defun binary-char>= (char1 char2)
  (>= (char-code char1) (char-code char2)))
