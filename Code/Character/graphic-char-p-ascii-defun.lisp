(cl:in-package #:sicl-character)

(defun graphic-char-p (character)
  (<= 32 (char-code character) 126))
