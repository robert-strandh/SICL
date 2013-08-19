(cl:in-package #:sicl-cons)

(defun consp (object)
  (sicl-type:typeq object cons))

(defun car (list)
  (declare (type list list))
  (if (consp list)
      (load-car list)
      nil))

(defun cdr (list)
  (declare (type list list))
  (if (consp list)
      (load-cdr list)
      nil))
  
(defun rplaca (cons object)
  (declare (type cons cons))
  (store-car cons object)
  cons)

(defun rplacd (cons object)
  (declare (type cons cons))
  (store-cdr cons object)
  cons)
