(cl:in-package #:sicl-setf)

;;; This code is in the public domain.
;;;
;;; The name for this project is SICL, which doesn't stand for
;;; anything in particular.  Pronounce it like "sickle".
;;;
;;; The purpose of this code is to provide a totally portable
;;; implementation of some high-level functionality of the Common Lisp
;;; language, so that implementors of Common Lisp systems can
;;; integrate it as it is into their systems, without having to
;;; implement and maintain a specific version of it.
;;;
;;; Author: Robert Strandh (robert.strandh@gmail.com)
;;; Date: 2008, 2015.
;;;
;;; SETF expanders for standard accessors that can be expressed
;;; by portable code.  For that to be possible, Common Lisp must
;;; also define primitive functions for storing values.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The car/cdr family

(define-setf-expander car (x)
  (let ((subform-temp (gensym))
	(store-temp (gensym)))
    (values (list subform-temp)
	    (list x)
	    (list store-temp)
	    `(progn (rplaca ,subform-temp ,store-temp) ,store-temp)
	    `(car ,subform-temp))))

(define-setf-expander cdr (x)
  (let ((subform-temp (gensym))
	(store-temp (gensym)))
    (values (list subform-temp)
	    (list x)
	    (list store-temp)
	    `(progn (rplacd ,subform-temp ,store-temp) ,store-temp)
	    `(cdr ,subform-temp))))

(defmacro define-car/cdr-expander (reader modifier subreader)
  `(define-setf-expander ,reader (x)
     (let ((subform-temp (gensym))
	   (store-temp (gensym)))
       (values (list subform-temp)
	       (list x)
	       (list store-temp)
	       `(progn (,',modifier (,',subreader ,subform-temp) ,store-temp) ,store-temp)
	       `(,',reader ,subform-temp)))))

(define-car/cdr-expander caar rplaca car)
(define-car/cdr-expander cadr rplaca cdr)
(define-car/cdr-expander cdar rplacd car)
(define-car/cdr-expander cddr rplacd cdr)
(define-car/cdr-expander caaar rplaca caar)
(define-car/cdr-expander caadr rplaca cadr)
(define-car/cdr-expander cadar rplaca cdar)
(define-car/cdr-expander caddr rplaca cddr)
(define-car/cdr-expander cdaar rplacd caar)
(define-car/cdr-expander cdadr rplacd cadr)
(define-car/cdr-expander cddar rplacd cdar)
(define-car/cdr-expander cdddr rplacd cddr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The first, second, etc family

(define-setf-expander first (x)
  (let ((subform-temp (gensym))
	(store-temp (gensym)))
    (values (list subform-temp)
	    (list x)
	    (list store-temp)
	    `(progn (rplaca ,subform-temp ,store-temp) ,store-temp)
	    `(car ,subform-temp))))

(defmacro define-nth-expander (reader n)
  `(define-setf-expander ,reader (x)
     (let ((subform-temp (gensym))
	   (store-temp (gensym)))
       (values (list subform-temp)
	       (list x)
	       (list store-temp)
	       `(progn (rplaca (nthcdr ,',n ,subform-temp) ,store-temp) ,store-temp)
	       `(,',reader ,subform-temp)))))

(define-nth-expander second 1)
(define-nth-expander third 2)
(define-nth-expander fourth 3)
(define-nth-expander fifth 4)
(define-nth-expander sixth 5)
(define-nth-expander seventh 6)
(define-nth-expander eighth 7)
(define-nth-expander ninth 8)
(define-nth-expander tenth 9)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The

(define-setf-expander the (value-type place &environment env)
  (multiple-value-bind (temps forms store-vars storing-form accessing-form)
      (get-setf-expansion place env)
    (values temps
	    forms
	    store-vars
	    `(multiple-value-bind ,store-vars
		 (the ,value-type (values ,store-vars))
	       ,storing-form)
	    `(the ,value-type ,accessing-form))))
