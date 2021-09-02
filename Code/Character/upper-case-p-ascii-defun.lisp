(cl:in-package #:sicl-character)

(defun upper-case-p (character)
  (<= 65 (char-code character) 90))
