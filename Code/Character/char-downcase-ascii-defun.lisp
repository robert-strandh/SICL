(cl:in-package #:sicl-character)

(defun char-downcase (character)
  (if (upper-case-p character)
      (code-char (+ (char-code character) (- 97 65)))
      character))
