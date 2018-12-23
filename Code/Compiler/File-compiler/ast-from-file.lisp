(cl:in-package #:sicl-file-compiler)

(defun ast-from-stream (stream environment system)
  (cleavir-ast:make-progn-ast
   (loop with cleavir-generate-ast:*compiler* = 'compile-file
         with eof = (list nil)
         for cst = (eclector.concrete-syntax-tree:cst-read stream nil eof)
         until (eq cst eof)
         collect (cleavir-cst-to-ast:cst-to-ast cst environment system))
   :origin nil
   :policy nil))

(defun ast-from-file (filename environment system)
  (with-open-file (stream filename :direction :input)
    (ast-from-stream stream environment system)))
