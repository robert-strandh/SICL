(cl:in-package #:sicl-character)

(defun char-not-equal (&rest characters)
  (when (null characters)
    (error 'program-error))
  (loop for (char1 . rest-chars) on characters
        repeat (1- (length characters))
        do (loop for char2 in rest-chars
                 when (binary-char-equal char1 char2)
                   do (return-from char-not-equal nil)))
  t)

(proclaim '(notinline char-not-equal))
