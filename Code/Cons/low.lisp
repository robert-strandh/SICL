(cl:in-package #:sicl-cons)

(defun consp (x)
  (sicl-type:typeq x cons))

(defun car (x)
  (declare (type list x))
  (if (consp x)
      (load-car x)
      nil))

(defun cdr (x)
  (declare (type list x))
  (if (consp x)
      (load-cdr x)
      nil))
  
(defun rplaca (cons object)
  (declare (type cons cons))
  (store-car cons object)
  cons)

(defun rplacd (cons object)
  (declare (type cons cons))
  (store-cdr cons object)
  cons)
