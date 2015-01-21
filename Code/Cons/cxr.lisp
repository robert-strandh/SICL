(cl:in-package #:sicl-cons)

(defun cadr (list)
  (car (cdr list)))

(defun cddr (list)
  (cdr (cdr list)))

(defun caar (list)
  (car (car list)))

(defun cdar (list)
  (cdr (car list)))

(defun caddr (list)
  (car (cddr list)))

(defun cdddr (list)
  (cdr (cddr list)))

(defun caadr (list)
  (car (cadr list)))

(defun cdadr (list)
  (cdr (cadr list)))

(defun cadar (list)
  (car (cdar list)))

(defun cddar (list)
  (cdr (cdar list)))

(defun caaar (list)
  (car (caar list)))

(defun cdaar (list)
  (cdr (caar list)))

(defun cadddr (list)
  (car (cdddr list)))

(defun cddddr (list)
  (cdr (cdddr list)))

(defun caaddr (list)
  (car (caddr list)))

(defun cdaddr (list)
  (cdr (caddr list)))

(defun cadadr (list)
  (car (cdadr list)))

(defun cddadr (list)
  (cdr (cdadr list)))

(defun caaadr (list)
  (car (caadr list)))

(defun cdaadr (list)
  (cdr (caadr list)))

(defun caddar (list)
  (car (cddar list)))

(defun cdddar (list)
  (cdr (cddar list)))

(defun caadar (list)
  (car (cadar list)))

(defun cdadar (list)
  (cdr (cadar list)))

(defun cadaar (list)
  (car (cdaar list)))

(defun cddaar (list)
  (cdr (cdaar list)))

(defun caaaar (list)
  (car (caaar list)))

(defun cdaaar (list)
  (cdr (caaar list)))

(defun first (list)
  (car list))

(defun second (list)
  (car (cdr list)))

(defun third (list)
  (car (cddr list)))

(defun fourth (list)
  (car (cdddr list)))

(defun fifth (list)
  (car (cddddr list)))

(defun sixth (list)
  (car (cdr (cddddr list))))

(defun seventh (list)
  (car (cddr (cddddr list))))

(defun eighth (list)
  (car (cdddr (cddddr list))))

(defun ninth (list)
  (car (cddddr (cddddr list))))

(defun tenth (list)
  (car (cdr (cddddr (cddddr list)))))

(defun rest (list)
  (cdr list))
