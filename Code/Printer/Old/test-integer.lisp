(cl:in-package #:sicl-printer)

(defun test ()
  (loop repeat 100000
        for base = (+ 2 (random 35))
        for number = (1+ (random (expt 2 234)))
        for string1 = (let ((*print-base* base)) (princ-to-string number))
        for string2 = (with-output-to-string (stream)
                        (print-positive-integer number base stream))
        always (string= string1 string2)))
