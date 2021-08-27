(cl:in-package #:sicl-character)

(defun lower-case-p (character)
  (<= 97 (char-code character) 122))
