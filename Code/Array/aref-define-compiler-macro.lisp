(cl:in-package #:sicl-array)

(define-compiler-macro aref (array &rest subscripts)
  (let ((array-var (gensym)))
    `(let ((,array-var ,array))
       (row-major-aref ,array-var
                       (array-row-major-index ,array-var
                                              ,@subscripts)))))
