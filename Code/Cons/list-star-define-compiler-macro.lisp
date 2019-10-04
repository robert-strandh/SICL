(cl:in-package #:sicl-cons)

(define-compiler-macro list* (&whole form &rest args)
  (cond  ((null args) form)
         ((null (cdr args)) (car args))
         (t `(cons ,(car args) (list* ,@(cdr args))))))
