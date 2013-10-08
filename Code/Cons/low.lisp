(cl:in-package #:sicl-cons)

(declaim (inline consp car cdr rplaca rplacd))

;;; It might seem backward to define CONSP in terms of TYPEP, but
;;; TYPEP with a quoted arguments is transformed by a compiler macro
;;; into (SICL-TYPE:TYPEQ OBJECT CONS), which is processed by the type
;;; inference mechanism.  Should there be any remaining instances of
;;; (SICL-TYPE:TYPEQ OBJECT CONS) when the type inference is finished,
;;; it is transformed into a low-level operation that tests the tag
;;; bits.  In the implementation of TYPEP, when it turns out that the
;;; type being tested for is CONS, there might very well be a call to
;;; CONSP, which might then look circular, but it really is not
;;; because of what was said above.
(defun consp (object)
  (typep object 'cons))

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
