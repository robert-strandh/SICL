(cl:in-package #:sicl-cons)

(defun cadr (list)
  (declare (type (or null (cons * (or null cons)))
		 list))
  (car (cdr list)))

(defun cddr (list)
  (declare (type (or null (cons * (or null cons)))
		 list))
  (cdr (cdr list)))

(defun caar (list)
  (declare (type (or null (cons (or null cons) *))
		 list))
  (car (car list)))

(defun cdar (list)
  (declare (type (or null (cons (or null cons) *))
		 list))
  (cdr (car list)))

(defun caddr (list)
  (declare (type (or null (cons * (or null (cons * (or null cons)))))
		 list)

	   