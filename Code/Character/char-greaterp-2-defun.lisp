(cl:in-package #:sicl-character)

(defun char-greaterp (&rest characters)
  (when (null characters)
    (error 'program-error))
  (if (null (cdr characters))
      t
      (loop for (char1 char2) on characters
            repeat (1- (length characters))
            unless (binary-char-greaterp char1 char2)
              return nil
            finally (return t))))
 
(proclaim '(notinline char-greaterp))
