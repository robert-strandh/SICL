(cl:in-package #:sicl-character)

(defun alpha-char-p (character)
  (unless (characterp character)
    (error 'type-error :datum character :expected-type 'character))
  (or (lower-case-p character) (upper-case-p character)))
