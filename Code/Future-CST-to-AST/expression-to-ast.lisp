(cl:in-package #:sicl-expression-to-ast)

(defun expression-to-ast
    (client cooked-expression environment &key file-compilation-semantics)
  (let ((*subforms-are-top-level-p* t)
        (*compile-time-too* nil)
        (*use-file-compilation-semantics-p* file-compilation-semantics))
    (convert client cooked-expression environment)))
