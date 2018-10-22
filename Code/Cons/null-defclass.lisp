(cl:in-package #:sicl-cons)

(defclass null (symbol list)
  (:metaclass built-in-class)
  (:default-initargs :name "NIL" :package (find-package '#:common-lisp)))
   
