(cl:in-package #:sicl-character)

(defun alpha-char-p (character)
  (unless (characterp character)
    (error 'type-error :datum character :expected-type 'character))
  (or (cl-unicode:has-property character "Letter")
      (cl-unicode:has-property character "ther_Alphabetic")))
