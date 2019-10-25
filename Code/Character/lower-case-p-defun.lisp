(cl:in-package #:sicl-character)

(defun lower-case-p (character)
  (cl-unicode:has-property character "lowercaseLetter"))
