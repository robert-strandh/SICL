(cl:in-package #:sicl-character)

(defun code-char (code)
  (check-type code (integer 0 #.(1- char-code-limit)))
  (if (not (<= #xd800 code #xdfff))
    (cleavir-primop:code-char code)
    nil))
