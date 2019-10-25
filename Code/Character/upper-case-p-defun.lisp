(cl:in-package #:sicl-character)

(defun upper-case-p (character)
  (cl-unicode:has-property character "UppercaseLetter"))
