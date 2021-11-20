(cl:in-package #:sicl-character)

(defun char/= (&rest characters)
  (when (null characters)
    (error 'program-error))
  (loop for (char1 . rest-chars) on characters
        repeat (1- (length characters))
        do (loop for char2 in rest-chars
                 when (binary-char= char1 char2)
                   do (return-from char/= nil)))
  t)

(proclaim '(notinline char/=))
