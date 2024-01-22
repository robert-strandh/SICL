(cl:in-package #:sicl-clos)

(defclass sequence (t)
  ()
  (:metaclass built-in-class))

(defclass list (sequence) ()
  (:metaclass built-in-class))

(defclass cons (list sequence t)
  ()
  (:metaclass built-in-class))

(defgeneric symbol-name (symbol))

(defgeneric symbol-package (symbol))

(defclass symbol ()
  ((%name
    :initarg :name
    :reader symbol-name)
   (%package
    :initarg :package
    :reader symbol-package)))

(defclass null (symbol list)
  ()
  (:default-initargs :name "NIL" :package (find-package '#:common-lisp)))
