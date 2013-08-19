(cl:in-package #:sicl-cons)

(deftype cxr (operations)
  (if (= (length operations) 1)
      'list
      (ecase (car (last operations))
	(a `(or null (cons (cxr ,(butlast operations)) *)))
	(d `(or null (cons * (cxr ,(butlast operations))))))))

(defun cadr (list)
  (declare (type (cxr (x d)) list))
  (car (cdr list)))

(defun cddr (list)
  (declare (type (cxr (x d)) list))
  (cdr (cdr list)))

(defun caar (list)
  (declare (type (cxr (x a)) list))
  (car (car list)))

(defun cdar (list)
  (declare (type (cxr (x a)) list))
  (cdr (car list)))

(defun caddr (list)
  (declare (type (cxr (x d d)) list))
  (car (cddr list)))

(defun cdddr (list)
  (declare (type (cxr (x d d)) list))
  (cdr (cddr list)))

(defun caadr (list)
  (declare (type (cxr (x a d)) list))
  (car (cadr list)))

(defun cdadr (list)
  (declare (type (cxr (x a d)) list))
  (cdr (cadr list)))

(defun cadar (list)
  (declare (type (cxr (x d a)) list))
  (car (cdar list)))

(defun cddar (list)
  (declare (type (cxr (x d a)) list))
  (cdr (cdar list)))

(defun caaar (list)
  (declare (type (cxr (x a a)) list))
  (car (caar list)))

(defun cdaar (list)
  (declare (type (cxr (x a a)) list))
  (cdr (caar list)))

(defun cadddr (list)
  (declare (type (cxr (x d d d)) list))
  (car (cdddr list)))

(defun cddddr (list)
  (declare (type (cxr (x d d d))
		 list))
  (cdr (cdddr list)))

(defun caaddr (list)
  (declare (type (cxr (x a d d)) list))
  (car (caddr list)))

(defun cdaddr (list)
  (declare (type (cxr (x a d d)) list))
  (cdr (caddr list)))

(defun cadadr (list)
  (declare (type (cxr (x d a d)) list))
  (car (cdadr list)))

(defun cddadr (list)
  (declare (type (cxr (x d a d)) list))
  (cdr (cdadr list)))

(defun caaadr (list)
  (declare (type (cxr (x a a d)) list))
  (car (caadr list)))

(defun cdaadr (list)
  (declare (type (cxr (x a a d)) list))
  (cdr (caadr list)))

(defun caddar (list)
  (declare (type (cxr (x d d a)) list))
  (car (cddar list)))

(defun cdddar (list)
  (declare (type (cxr (x d d a)) list))
  (cdr (cddar list)))

(defun caadar (list)
  (declare (type (cxr (x a d a)) list))
  (car (cadar list)))

(defun cdadar (list)
  (declare (type (cxr (x a d a)) list))
  (cdr (cadar list)))

(defun cadaar (list)
  (declare (type (cxr (x d a a)) list))
  (car (cdaar list)))

(defun cddaar (list)
  (declare (type (cxr (x d a a)) list))
  (cdr (cdaar list)))

(defun caaaar (list)
  (declare (type (cxr (x a a a)) list))
  (car (caaar list)))

(defun cdaaar (list)
  (declare (type (cxr (x a a a)) list))
  (cdr (caaar list)))

(defun first (list)
  (declare (type (cxr (x)) list))
  (car list))

(defun second (list)
  (declare (type (cxr (x d)) list))
  (car (cdr list)))

(defun third (list)
  (declare (type (cxr (x d d)) list))
  (car (cddr list)))

(defun fourth (list)
  (declare (type (cxr (x d d d)) list))
  (car (cdddr list)))

(defun fifth (list)
  (declare (type (cxr (x d d d d)) list))
  (car (cddddr list)))

(defun sixth (list)
  (declare (type (cxr (x d d d d d)) list))
  (car (cdr (cddddr list))))

(defun seventh (list)
  (declare (type (cxr (x d d d d d d)) list))
  (car (cddr (cddddr list))))

(defun eighth (list)
  (declare (type (cxr (x d d d d d d d)) list))
  (car (cdddr (cddddr list))))

(defun ninth (list)
  (declare (type (cxr (x d d d d d d d d)) list))
  (car (cddddr (cddddr list))))

(defun tenth (list)
  (declare (type (cxr (x d d d d d d d d)) list))
  (car (cdr (cddddr (cddddr list)))))

(defun rest (list)
  (declare (type (crx (x)) list)))
