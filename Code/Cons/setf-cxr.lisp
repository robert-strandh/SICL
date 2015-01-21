(cl:in-package #:sicl-cons)

(defun (setf car) (new-value cons)
  (rplaca cons new-value)
  new-value)

(defun (setf cdr) (new-value cons)
  (rplacd cons new-value)
  new-value)

(defun (setf caar) (new-value cons)
  (setf (car (car cons)) new-value))

(defun (setf cadr) (new-value cons)
  (setf (car (cdr cons)) new-value))

(defun (setf cdar) (new-value cons)
  (setf (cdr (car cons)) new-value))

(defun (setf cddr) (new-value cons)
  (setf (cdr (cdr cons)) new-value))

(defun (setf caaar) (new-value cons)
  (setf (car (caar cons)) new-value))

(defun (setf caadr) (new-value cons)
  (setf (car (cadr cons)) new-value))

(defun (setf cadar) (new-value cons)
  (setf (car (cdar cons)) new-value))

(defun (setf caddr) (new-value cons)
  (setf (car (cddr cons)) new-value))

(defun (setf cdaar) (new-value cons)
  (setf (cdr (caar cons)) new-value))

(defun (setf cdadr) (new-value cons)
  (setf (cdr (cadr cons)) new-value))

(defun (setf cddar) (new-value cons)
  (setf (cdr (cdar cons)) new-value))

(defun (setf cdddr) (new-value cons)
  (setf (cdr (cddr cons)) new-value))

(defun (setf caaaar) (new-value cons)
  (setf (car (caaar cons)) new-value))

(defun (setf caaadr) (new-value cons)
  (setf (car (caadr cons)) new-value))

(defun (setf caadar) (new-value cons)
  (setf (car (cadar cons)) new-value))

(defun (setf caaddr) (new-value cons)
  (setf (car (caddr cons)) new-value))

(defun (setf cadaar) (new-value cons)
  (setf (car (cdaar cons)) new-value))

(defun (setf cadadr) (new-value cons)
  (setf (car (cdadr cons)) new-value))

(defun (setf caddar) (new-value cons)
  (setf (car (cddar cons)) new-value))

(defun (setf cadddr) (new-value cons)
  (setf (car (cdddr cons)) new-value))

(defun (setf cdaaar) (new-value cons)
  (setf (cdr (caaar cons)) new-value))

(defun (setf cdaadr) (new-value cons)
  (setf (cdr (caadr cons)) new-value))

(defun (setf cdadar) (new-value cons)
  (setf (cdr (cadar cons)) new-value))

(defun (setf cdaddr) (new-value cons)
  (setf (cdr (caddr cons)) new-value))

(defun (setf cddaar) (new-value cons)
  (setf (cdr (cdaar cons)) new-value))

(defun (setf cddadr) (new-value cons)
  (setf (cdr (cdadr cons)) new-value))

(defun (setf cdddar) (new-value cons)
  (setf (cdr (cddar cons)) new-value))

(defun (setf cddddr) (new-value cons)
  (setf (cdr (cdddr cons)) new-value))

(defun (setf first) (new-value cons)
  (setf (car cons) new-value))

(defun (setf second) (new-value cons)
  (setf (car (cdr cons)) new-value))

(defun (setf third) (new-value cons)
  (setf (car (cddr cons)) new-value))

(defun (setf fourth) (new-value cons)
  (setf (car (cdddr cons)) new-value))

(defun (setf fifth) (new-value cons)
  (setf (car (cddddr cons)) new-value))

(defun (setf sixth) (new-value cons)
  (setf (car (cdr (cddddr cons))) new-value))

(defun (setf seventh) (new-value cons)
  (setf (car (cddr (cddddr cons))) new-value))

(defun (setf eighth) (new-value cons)
  (setf (car (cdddr (cddddr cons))) new-value))

(defun (setf ninth) (new-value cons)
  (setf (car (cddddr (cddddr cons))) new-value))

(defun (setf tenth) (new-value cons)
  (setf (car (cdr (cddddr (cddddr cons)))) new-value))

(defun (setf rest) (new-value cons)
  (setf (cdr cons) new-value))
