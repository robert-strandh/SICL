(cl:in-package #:sicl-character)

(defun char-upcase (character)
  (if (lower-case-p character)
      (code-char (- (char-code character) (- 97 65)))
      character))
