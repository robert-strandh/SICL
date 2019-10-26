(cl:in-package #:sicl-character)

(defun character-weight (character)
  (let ((code (char-code (char-upcase character))))
    (cond ((<= (char-code #\0) code (char-code #\9))
           (- code (char-code #\0)))
          ((<= (char-code #\A) code (char-code #\Z))
           (+ 10 (- code (char-code #\A))))
          (t
           nil))))

(defun digit-char-p (character &optional (radix 10))
  (unless (<= 2 radix 36)
    (error 'type-error
           :datum radix
           :expected-type '(integer 2 36)))
  (let ((weight (character-weight character)))
    (if (or (null weight) (>= weight radix))
        nil
        weight)))
