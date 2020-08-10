(cl:in-package #:sicl-character)

(defun code-char (code)
  (if (typep code '(integer 0 #xffffff))
      (cleavir-primop:code-char code)
      (error 'type-error
             :datum code
             :expected-type '(integer 0 #xffffff))))
