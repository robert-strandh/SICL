(cl:in-package #:sicl-expression-to-ast)

(defun cst-to-ast (client cst environment &key file-compilation-semantics)
  (let ((*subforms-are-top-level-p* t)
        (*compile-time-too* nil)
        (*use-file-compilation-semantics-p* file-compilation-semantics))
    (convert client cst environment)))
