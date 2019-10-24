(cl:in-package #:sicl-character)

(defun binary-char-greaterp (char1 char2)
  (> (char-code (char-upcase char1)) (char-code (char-upcase char2))))
