(cl:in-package #:sicl-boot)

;;; Return the number words in the prefix of a rack that contains
;;; words that with Lisp objects in them, as opposed to raw data.
(defgeneric trace-prefix (object))
