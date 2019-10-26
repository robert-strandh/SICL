(cl:in-package #:sicl-character)

(defun digit-char (weight &optional (radix 10))
  (unless (typep radix '(integer 2 36))
    (error 'type-error
           :datum radix
           :expected-type '(integer 2 36)))
  (unless (typep weight '(integer 0))
    (error 'type-error
           :datum weight
           :expected-type '(integer 0)))
  (cond ((>= weight radix)
         nil)
        ((<= weight 9)
         (code-char (+ (char-code #\0) weight)))
        (t
         (code char (+ (char-code #\A) (- weight 10))))))
