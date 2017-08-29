(cl:in-package #:cleavir-cst-to-ast-test)

(defun ast-from-string (string)
  (let ((*readtable* cleavir-io:*io-readtable*)
        (cleavir-ast:*policy* nil))
    (read-from-string string)))
