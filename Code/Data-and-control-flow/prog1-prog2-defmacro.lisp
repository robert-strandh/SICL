(cl:in-package #:sicl-data-and-control-flow)

(defmacro prog1 (first-form &rest forms)
  (let ((temp-var (gensym)))
    `(let ((,temp-var ,first-form))
       ,@forms
       ,temp-var)))

(defmacro prog2 (first-form second-form &rest forms)
  (let ((temp-var (gensym)))
    `(progn
       ,first-form
       (let ((,temp-var ,second-form))
         ,@forms
         ,temp-var))))
