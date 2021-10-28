(cl:in-package #:sicl-string)

(defgeneric char (string index))

(defmethod char ((string string) index)
  (aref string index))

(defgeneric schar (string index))

;;; FIXME: this function should not ignore the fill pointer.
(defmethod schar ((string string) index)
  (aref string index))
