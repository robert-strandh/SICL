(cl:in-package #:sicl-compiler)

(defun read-cst (input-stream eof-marker)
  (eclector.concrete-syntax-tree:read input-stream nil eof-marker))

(defun cst-to-ast (client cst compilation-environment)
  (cleavir-cst-to-ast:cst-to-ast
   client cst compilation-environment :file-compilation-semantics t))

(defun ast-from-stream (client input-stream compilation-environment)
  (let* ((*package* *package*)
         (asts
           (loop with eof-marker = input-stream
                 for cst = (read-cst input-stream eof-marker)
                 until (eq cst eof-marker)
                 collect (cst-to-ast client cst compilation-environment))))
    (cleavir-ast:make-ast 'cleavir-ast:progn-ast
      :origin nil
      :form-asts asts)))

(defun ast-from-file (client absolute-pathname compilation-environment)
  (sicl-source-tracking:with-source-tracking-stream-from-file
      (input-stream absolute-pathname)
    (ast-from-stream client input-stream compilation-environment)))
