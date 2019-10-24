(cl:in-package #:sicl-character)

(defun char> (&rest characters)
  (when (null characters)
    (error 'program-error))
  (if (null (cdr characters))
      t
      (loop for rest = (cdr characters) then (cdr rest)
            while (consp rest)
            for char1 = (car characters) then char2
            for char2 = (car rest)
            unless (binary-char> char1 char2)
              return nil
            finally (return t))))
 
(proclaim '(notinline char>))
