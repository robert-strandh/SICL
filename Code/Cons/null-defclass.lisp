(cl:in-package #:sicl-cons)

(defclass null (symbol list)
  (:default-initargs :name "NIL" :package (find-package '#:common-lisp)))
   
