(cl:in-package #:target)

(setf (macro-function 'defmacro)
      (macro-function 'cl:defmacro))

(defmacro defmacro (name lambda-list &body body)
  `(setf (macro-function ',name)
         ,(ecclesia:parse-macro-using-canonicalization name lambda-list body)))

(defmacro when (test &body body)
  `(if ,test (progn ,@body)))

(defmacro defun (name lambda-list &body body)
  `(setf (fdefinition ',name)
         (lambda ,lambda-list
           ,@body)))
