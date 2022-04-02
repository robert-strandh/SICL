(cl:in-package #:sicl-compiler)

(defun read-cst (input-stream eof-marker)
  (eclector.concrete-syntax-tree:read input-stream nil eof-marker))

(defun cst-to-ast (client cst compilation-environment)
  (handler-bind
      ((trucler:undefined-function-referred-to-by-inline-declaration
         (lambda (condition)
           (let ((*package* (find-package "KEYWORD")))
             (warn 'undefined-function
                   :source-location (trucler:origin condition)
                   :name (trucler:name condition)))
           (invoke-restart 'trucler:ignore-declaration)))
       (trucler:no-function-description
         (lambda (condition)
           (let ((*package* (find-package "KEYWORD")))
             (warn 'undefined-function
                   :source-location (trucler:origin condition)
                   :name (trucler:name condition)))
           (invoke-restart 'cleavir-cst-to-ast:consider-global)))
       (trucler:no-variable-description
         (lambda (condition)
           (let ((*package* (find-package "KEYWORD")))
             (warn 'undefined-variable
                   :source-location (trucler:origin condition)
                   :name (trucler:name condition)))
           (invoke-restart 'cleavir-cst-to-ast:consider-special)))
       (trucler:no-block-description
         (lambda (condition)
           (let ((*package* (find-package "KEYWORD")))
             (warn 'undefined-block
                   :source-location (trucler:origin condition)
                   :name (trucler:name condition)))
           (invoke-restart 'continue)))
       (trucler:no-tag-description
         (lambda (condition)
           (let ((*package* (find-package "KEYWORD")))
             (warn 'undefined-block
                   :source-location (trucler:origin condition)
                   :name (trucler:name condition)))
           (invoke-restart 'continue))))
    (sicl-cst-to-ast:cst-to-ast client cst compilation-environment t)))

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
