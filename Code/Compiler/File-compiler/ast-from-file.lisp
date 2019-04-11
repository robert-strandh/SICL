(cl:in-package #:sicl-file-compiler)

(defun ast-from-stream (stream environment)
  (cleavir-ast:make-progn-ast
   (loop with cleavir-generate-ast:*compiler* = 'compile-file
         with client = (make-instance 'sicl-file-compiler)
         with eof = (list nil)
         for cst = (eclector.concrete-syntax-tree:cst-read stream nil eof)
         until (eq cst eof)
         collect (cleavir-cst-to-ast:cst-to-ast cst environment client))
   :origin nil
   :policy nil))

(defun ast-from-file (filename environment)
  (with-open-file (stream filename :direction :input)
    (ast-from-stream stream environment)))
