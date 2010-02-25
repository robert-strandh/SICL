(in-package :sicl.cons.high)

(defun (setf car) (object cons)
  (rplaca cons object)
  object)

(defun (setf cdr) (object cons)
  (rplacd cons object)
  object)

(defun caar (x)
  (car (car x)))

(defun (setf caar) (object cons)
  (setf (car (car cons)) object)
  object)

(defun cadr (x)
  (car (cdr x)))

(defun (setf cadr) (object cons)
  (setf (car (cdr cons)) object)
  object)

(defun cdar (x)
  (cdr (car x)))

(defun (setf cdar) (object cons)
  (setf (cdr (car cons)) object)
  object)

(defun cddr (x)
  (cdr (cdr x)))

(defun (setf cddr) (object cons)
  (setf (cdr (cdr cons)) object)
  object)

(defun caaar (x)
  (car (caar x)))

(defun (setf caaar) (object cons)
  (setf (car (caar cons)) object)
  object)

(defun caadr (x)
  (car (cadr x)))

(defun (setf caadr) (object cons)
  (setf (car (cadr cons)) object)
  object)

(defun cadar (x)
  (car (cdar x)))

(defun (setf cadar) (object cons)
  (setf (car (cdar cons)) object)
  object)

(defun caddr (x)
  (car (cddr x)))

(defun (setf caddr) (object cons)
  (setf (car (cddr cons)) object)
  object)

(defun cdaar (x)
  (cdr (caar x)))

(defun (setf cdaar) (object cons)
  (setf (cdr (caar cons)) object)
  object)

(defun cdadr (x)
  (cdr (cadr x)))

(defun (setf cdadr) (object cons)
  (setf (cdr (cadr cons)) object)
  object)

(defun cddar (x)
  (cdr (cdar x)))

(defun (setf cddar) (object cons)
  (setf (cdr (cdar cons)) object)
  object)

(defun cdddr (x)
  (cdr (cddr x)))

(defun (setf cdddr) (object cons)
  (setf (cdr (cddr cons)) object)
  object)

(defun caaaar (x)
  (car (caaar x)))

(defun (setf caaaar) (object cons)
  (setf (car (caaar cons)) object)
  object)

(defun caaadr (x)
  (car (caadr x)))

(defun (setf caaadr) (object cons)
  (setf (car (caadr cons)) object)
  object)

(defun caadar (x)
  (car (cadar x)))

(defun (setf caadar) (object cons)
  (setf (car (cadar cons)) object)
  object)

(defun caaddr (x)
  (car (caddr x)))

(defun (setf caaddr) (object cons)
  (setf (car (caddr cons)) object)
  object)

(defun cadaar (x)
  (car (cdaar x)))

(defun (setf cadaar) (object cons)
  (setf (car (cdaar cons)) object)
  object)

(defun cadadr (x)
  (car (cdadr x)))

(defun (setf cadadr) (object cons)
  (setf (car (cdadr cons)) object)
  object)

(defun caddar (x)
  (car (cddar x)))

(defun (setf caddar) (object cons)
  (setf (car (cddar cons)) object)
  object)

(defun cadddr (x)
  (car (cdddr x)))

(defun (setf cadddr) (object cons)
  (setf (car (cdddr cons)) object)
  object)

(defun cdaaar (x)
  (cdr (caaar x)))

(defun (setf cdaaar) (object cons)
  (setf (cdr (caaar cons)) object)
  object)

(defun cdaadr (x)
  (cdr (caadr x)))

(defun (setf cdaadr) (object cons)
  (setf (cdr (caadr cons)) object)
  object)

(defun cdadar (x)
  (cdr (cadar x)))

(defun (setf cdadar) (object cons)
  (setf (cdr (cadar cons)) object)
  object)

(defun cdaddr (x)
  (cdr (caddr x)))

(defun (setf cdaddr) (object cons)
  (setf (cdr (caddr cons)) object)
  object)

(defun cddaar (x)
  (cdr (cdaar x)))

(defun (setf cddaar) (object cons)
  (setf (cdr (cdaar cons)) object)
  object)

(defun cddadr (x)
  (cdr (cdadr x)))

(defun (setf cddadr) (object cons)
  (setf (cdr (cdadr cons)) object)
  object)

(defun cdddar (x)
  (cdr (cddar x)))

(defun (setf cdddar) (object cons)
  (setf (cdr (cddar cons)) object)
  object)

(defun cddddr (x)
  (cdr (cdddr x)))

(defun (setf cddddr) (object cons)
  (setf (cdr (cdddr cons)) object)
  object)

(defun first (x)
  (car x))

(defun (setf first) (object cons)
  (setf (car cons) object)
  object)

(defun second (x)
  (cadr x))

(defun (setf second) (object cons)
  (setf (cadr cons) object)
  object)

(defun third (x)
  (caddr x))

(defun (setf third) (object cons)
  (setf (caddr cons) object)
  object)

(defun fourth (x)
  (cadddr x))

(defun (setf fourth) (object cons)
  (setf (cadddr cons) object)
  object)

(defun fifth (x)
  (car (cddddr x)))

(defun (setf fifth) (object cons)
  (setf (car (cddddr xcons) object)
	object)

(defun sixth (x)
  (cadr (cddddr x)))

(defun (setf sixth) (object cons)
  (setf (cadr (cddddr xcons) object)
	object)

(defun seventh (x)
  (caddr (cddddr x)))

(defun (setf seventh) (object cons)
  (setf (caddr (cddddr xcons) object)
	object)

(defun eighth (x)
  (cadddr (cddddr x)))

(defun (setf eighth) (object cons)
  (setf (cadddr (cddddr xcons) object)
	object)

(defun nineth (x)
  (car (cddddr (cddddr x))))

(defun (setf nineth) (object cons)
  (setf (car (cddddr (cddddr x)cons) object)
	object)

(defun tenth (x)
  (cadr (cddddr (cddddr x))))

(defun (setf tenth) (object cons)
  (setf (cadr (cddddr (cddddr x)cons) object)
	object)

;;; this implementation assumes that there is no 
;;; structure sharing between the &rest argument
;;; and the last argument to apply
(defun list (&rest elements)
  elements)

(define-compiler-macro list (&rest args)
  (if (null args)
      'nil
      `(cons ,(car args) (list ,@(cdr args)))))

;;; this implementation assumes that there is no 
;;; structure sharing between the &rest argument
;;; and the last argument to apply
;;; TODO: fix the assert to signal a particular condition
(defun list* (&rest elements)
  (assert (not (null elements)))
  (if (null (cdr elements))
      (car elements)
      (loop for remaining on elements
	    until (null (cddr remaining))
	    finally (setf (cdr remaining)
			  (cadr remaining)))))

(define-compiler-macro list* (&whole form &rest args)
  (cond  ((null args) form)
	 ((null (cdr args)) (car args))
	 (t `(cons ,(car args) (list* ,@(cdr args))))))

(defun last (list &optional (n 1))
  (check-type list list "a list")
  (check-type n (and integer (not (satisfies minusp))) "a nonnegative integer")
  (let ((remaining list))
    (loop repeat n
	  until (atom remaining)
	  do (pop remaining))
    (loop until (atom remaining)
	  do (pop list)
	  do (pop remaining))
    list))

;;; special version of last used when the second argument to
;;; last is 1. 
(defun last-1 (list)
  (assert (listp list)
	  (list)
	  must-be-list :datum list)
  (loop for rest on list
	do (setf list rest))
  list)

(define-compiler-macro last (&whole form list &optional (n 1))
  (if (eql n 1)
      `(last-1 ,list)
      form))

(defun copy-list (list)
  (assert (listp list))
  (if (null list)
      nil
      (let* ((result (list (car list)))
	     (trailer result))
	(loop do (pop list)
	      until (atom list)
	      ;; list is not an atom, allocate a new cell
	      ;; at the end of the result list
	      do (setf (cdr trailer) (list (car list)))
	      do (pop trailer))
	;; when we come here, list is an atom,
	;; either NIL because the list was a proper list
	;; or some other atom because it was not a proper list
	(setf (cdr trailer) list)
	result)))

;;; The standard requires the argument to be either a circular list
;;; or a proper list.  In case of a circular list, NIL should be 
;;; returned.  
;;; FIXME make sure a type error is returned if the list is a dotted list. 
(defun list-length (list)
  (loop for length from 0 by 2
	for slow on list
	for fast on list by #'cddr
	do (cond ((endp (cdr fast)) (return (1+ length)))
		 ((and (eq fast slow) (plusp length)) (return nil)))
	finally (assert (listp fast)) (return length)))

(defun make-list (length &key (initial-element nil))
  (assert (and (integerp length) (>= length 0)))
  (loop repeat length
	collect initial-element))

(defun nthcdr (n list)
  (assert (and (integerp n) (>= n 0)))
  (loop repeat n
	until (null list)
	do (pop list))
  list)

(defun nth (n list)
  (car (nthcdr n list)))

(defun (setf nth) (object n list)
  (setf (car (nthcdr n list)) object))
