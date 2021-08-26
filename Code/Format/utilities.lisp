(cl:in-package #:sicl-format)

;;; For certain common types used by FORMAT, return a string
;;; explaining in English what the type means.  For other
;;; types, return a string "an object of type <type>"
(defun type-name (type)
  (cond ((symbolp type)
         (case type
           (integer "an integer")
           (character "a character")
           (list "a list")
           (t (format nil "an object of type ~s" type))))
        ((and (consp type) (eq (car type) 'integer))
         (case (length type)
           (1 "an integer")
           (2 (case (second type)
                (0 "a nonnegative integer")
                (1 "a strictly positive integer")
                (t (format nil "an integer greater than or equal to ~d" (second type)))))
           (3 (format nil "an integer between ~d and ~d" (second type) (third type)))
           (t (format nil "an object of type ~s" type))))
        (t (format nil "an object of type ~s" type))))
