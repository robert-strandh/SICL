(cl:in-package #:sicl-character)

(defun alphanumericp (character)
  (or (alpha-char-p character)
      (digit-char-p character)))
