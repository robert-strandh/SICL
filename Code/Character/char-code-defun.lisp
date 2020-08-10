(cl:in-package #:sicl-character)

(defun char-code (char)
  (if (characterp char)
      (cleavir-primop:char-code char)
      (error 'type-error
             :datum char
             :expected-type 'character)))
