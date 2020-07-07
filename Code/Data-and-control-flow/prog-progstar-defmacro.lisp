(cl:in-package #:sicl-data-and-control-flow)

(defmacro prog (bindings &body body)
  (multiple-value-bind (declarations items)
      (cleavir-code-utilities:separate-ordinary-body body)
    `(block nil
       (let ,bindings
         ,@declarations
         (tagbody ,@items)))))

(defmacro prog* (bindings &body body)
  (multiple-value-bind (declarations items)
      (cleavir-code-utilities:separate-ordinary-body body)
    `(block nil
       (let* ,bindings
         ,@declarations
         (tagbody ,@items)))))
