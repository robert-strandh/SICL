(cl:in-package #:sicl-expression-to-ast)

(defun expression-to-ast
    (client cooked-expression environment &key file-compilation-semantics)
  (let ((*subforms-are-top-level-p* t)
        (*compile-time-too* nil)
        (*use-file-compilation-semantics-p* file-compilation-semantics))
    (convert client cooked-expression environment)))

(defclass s-expression-client
    (clearcut-implementation-s-expression:client
     trucler-reference:client)
  ())

(defun s-expression-to-ast
    (s-expression environment &key file-compilation-semantics)
  (let ((client (make-instance 's-expression-client)))
    (let ((c:*client* client))
      (expression-to-ast
       client s-expression environment
       :file-compilation-semantics file-compilation-semantics))))

(defclass cst-client
    (clearcut-implementation-concrete-syntax-tree:client
     trucler-reference:client)
  ())

(defun cst-to-ast
    (s-expression environment &key file-compilation-semantics)
  (let ((client (make-instance 'cst-client)))
    (let ((c:*client* client))
      (expression-to-ast
       client s-expression environment
       :file-compilation-semantics file-compilation-semantics))))
