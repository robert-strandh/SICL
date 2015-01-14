(cl:in-package #:sicl-reader)

;;; As recommended by the HyperSpec, the backquote and comma reader
;;; macros generate a data structure that corresponds to the one used
;;; in Scheme, i.e. containing the forms (QUASIQUOTE <form>),
;;; (UNQUOTE <form>), and (UNQUOTE-SPLICING <form>).
;;;
;;; These forms are converted to standard Common Lisp forms
;;; containing APPEND, LIST, QUOTE, APPLY, and VECTOR. 
;;;
;;; Our implementation is an almost immediate translation of section
;;; 2.4.6 of the Hyperspec.  Where the HyperSpec uses brackets [FORM],
;;; we use (TRANSFORM FORM).  The other difference is that the
;;; HyperSpec converts a form such as `(x1 x2 ... xn . atom) directly
;;; to (append [x1] [x2] ... [xn] (quote atom)), whereas we do it
;;; indirectly by calling TRANSFORM-COMPOUND on the (potentially
;;; dottet) list. 

(defun transform (form)
  (if (consp form)
      (case (car form)
        (unquote
         `(list ,(cadr form)))
        (unquote-splicing
         (cadr form))
        (t
         `(list ,(transform-quasiquote-argument form))))
      `(list ,(transform-quasiquote-argument form))))

(defun transform-compound (compound)
  (if (atom compound)
      `((quote ,compound))
      (cons (transform (first compound)) (transform-compound (rest compound)))))

(defun transform-quasiquote-argument (argument)
  (cond ((consp argument)
	 (case (car argument)
	   (unquote
	    (cadr argument))
	   (unquote-splicing
	    ;; Hmm.  This condition type is a subclass of
	    ;; reader-error, which should be given a stream, but at
	    ;; this point we no longer have the stream available.
	    (error 'undefined-use-of-backquote))
	   (t
	    `(append ,@(transform-compound argument)))))
	((vectorp argument)
	 `(apply #'vector
		 ,(transform-quasiquote-argument
		   (coerce argument 'list))))
	(t
	 `(quote ,argument))))

(defun expand (form)
  (if (atom form)
      form
      (let ((expanded (cons (expand (car form)) (expand (cdr form)))))
	(if (eq (first expanded) 'quasiquote)
	    (transform-quasiquote-argument (second expanded))
	    expanded))))

(defmacro quasiquote (&whole form argument)
  (declare (ignore argument))
  (expand form))
