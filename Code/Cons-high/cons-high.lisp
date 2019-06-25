(cl:in-package :sicl-cons-high)

;;;; Copyright (c) 2008 - 2015
;;;;
;;;;     Robert Strandh (robert.strandh@gmail.com)
;;;;
;;;; all rights reserved. 
;;;;
;;;; Permission is hereby granted to use this software for any 
;;;; purpose, including using, modifying, and redistributing it.
;;;;
;;;; The software is provided "as-is" with no warranty.  The user of
;;;; this software assumes any responsibility of the consequences. 

;;;; This file is part of the cons-high module of the SICL project.
;;;; See the file SICL.text for a description of the project. 
;;;; See the file cons-high.text for a description of the module.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function list

;;; We need the list function in the implementation of other functions
;;; and or macros in this module, so we define it early.

;;; this implementation assumes that there is no 
;;; structure sharing between the &rest argument
;;; and the last argument to apply

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun list (&rest elements)
    elements))

(define-compiler-macro list (&rest args)
  (if (null args)
      'nil
      `(cons ,(car args) (list ,@(cdr args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Local function REVERSE-LIST.
;;;
;;; FIXME: signal an error if the list is not a proper list.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun reverse-list (list)
    (loop with result = '()
	  for rest = list then (cdr rest)
	  while (consp rest)
	  do (setf result (cons (car rest) result))
	  finally (return result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function append

;;; We need the append function in macro expanders in this module
;;; so we define it early.  

;;; It used to be the case that the append function was defined
;;; in terms of (loop ... append ...), but it turns out that
;;; some implementations have buggy versions of (loop ... append ...) 
;;; so it is better to just define it in terms of a lower-level
;;; construct.  Doing it this way allows for implementations with 
;;; a buggy loop to still use this module without first replacing
;;; loop by ours. 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun append (&rest lists)
    (if (null lists)
	;; The standard doesn't say explicity that calling append with
	;; zero arguments is allowed and returns nil, but one of the
	;; examples on the HyperSpec page for append suggests that. 
	'()
	;; We need to copy every list except the last one.  Also,
	;; every list except the last one must be a proper list.  In
	;; order to avoid making a test in each iteration of the loop,
	;; we use a sentinel, which is a cons cell whose cdr contains
	;; the real result that we return in the end. 
	(let* ((sentinel (list nil))
	       ;; The variable last points to the last cons cell of the
	       ;; resulting list to be accumulated. 
	       (last sentinel))
	  (loop until (null (cdr lists))
		do (loop with list = (car lists)
			 do (cond ((consp list)
				   ;; There are more cells, copy the first one.
				   (setf (cdr last) (list (car list)))
				   (setf last (cdr last))
				   (setf list (cdr list)))
				  ((null list)
				   ;; The list is a proper list and we reached the
				   ;; end of it.
				   (loop-finish))
				  (t
				   ;; The list is a dotted list
				   (error 'must-be-proper-list
					  :datum (car lists)
					  :name 'append))))
		   ;; We are through with the first list in lists
		   (setf lists (cdr lists)))
	  ;; When we get here, there is only one list left in lists.
	  ;; And in fact, it doesn't have to be a list at all. 
	  ;; Attach it to the end of what we have accumulated so far.
	  (setf (cdr last) (car lists))
	  ;; Skip the sentinel and return the rest. 
	  (cdr sentinel)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function mapcar
;;;
;;; The compiler macro is defined later because it uses the append
;;; function that may not be defined at this point.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mapcar (function &rest lists)
    (when (null lists)
      (error 'at-least-one-list-required :name 'mapcar))
    (loop for remaining = lists
	    then (loop for list in remaining collect (cdr list))
	  until (loop for list in remaining thereis (atom list))
	  collect (apply function
			 (loop for list in remaining collect (car list)))
	  finally (loop for rem in remaining
			for list in lists
			do (when (not (listp rem))
			     (error 'must-be-proper-list
				    :datum list
				    :name 'mapcar))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nconc

;;; It used to be the case that the append function was defined
;;; in terms of (loop ... nconc ...), but it turns out that
;;; some implementations have buggy versions of (loop ... nconc ...) 
;;; so it is better to just define it in terms of a lower-level
;;; construct.  Doing it this way allows for implementations with 
;;; a buggy loop to still use this module without first replacing
;;; loop by ours. 

(defun nconc (&rest lists)
  (if (null lists)
      nil
      ;; As with append, we use a sentinel to avoid a test in a loop. 
      (let* ((sentinel (list nil))
	     ;; The variable last is a pointer to the last cell of the
	     ;; accumulated list. 
	     (last sentinel))
	(loop for remaining = lists then (cdr remaining)
	      ;; Stop when we have processed every list except the last
	      ;; (which may be any object). 
	      until (null (cdr remaining))
	      do (let ((list (car remaining)))
		   (cond ((consp list)
			  ;; This is the normal case, the list has at
			  ;; least one cons cell, but may be proper or
			  ;; dotted. 
			  ;; 
			  ;; Hook it up to the end of the accumulated
			  ;; list.
			  (setf (cdr last) list)
			  ;; And move to the next item, making sure
			  ;; that last is right behind list. 
			  (setf last list)
			  (setf list (cdr list))
			  ;; Continue this process until list reaches
			  ;; an atom.  What that happens, last points
			  ;; to the last cons cell of the list, which
			  ;; is exactly what we need for next
			  ;; iteration.
			  (loop while (consp list)
				do (setf last list)
				   (setf list (cdr list))))
			 ((null list)
			  ;; If the list is nil, we just skip it. 
			  nil)
			 (t
			  ;; It is neither a cons nor nil, so we
			  ;; signal an error. 
			  (error 'must-be-list
				 :datum (car remaining)
				 :name 'nconc))))
	      finally (progn
			;; Hook up the last list to the end
			;; of what we have accumulated
			(setf (cdr last) (car remaining))))
	;; Skip the sentinel and return the rest. 
	(cdr sentinel))))

;;; FIXME: use a better condition that has both the 
;;; original argument at the part of the tree that 
;;; generated the error.
(defmacro define-c*r-function (function-name letters)
  (labels ((primitive (letter)
             (if (eql letter #\A) 'car 'cdr))
           (one-iteration (letter)
             `(cond ((consp remaining)
                     (setf remaining
                           (,(primitive letter) remaining)))
                    ((null remaining)
                     (return-from ,function-name nil))
                    (t
                     (error 'must-be-list
                            :datum remaining
                            :name ',function-name)))))
    `(defun ,function-name (list)
       (let ((remaining list))
         ,@(loop for letter in (reverse-list letters)
                 collect (one-iteration letter))
         remaining))))

(define-c*r-function caar   (#\A #\A))
(define-c*r-function cadr   (#\A #\D))
(define-c*r-function cdar   (#\D #\A))
(define-c*r-function cddr   (#\D #\D))
(define-c*r-function caaar  (#\A #\A #\A))
(define-c*r-function caadr  (#\A #\A #\D))
(define-c*r-function cadar  (#\A #\D #\A))
(define-c*r-function caddr  (#\A #\D #\D))
(define-c*r-function cdaar  (#\D #\A #\A))
(define-c*r-function cdadr  (#\D #\A #\D))
(define-c*r-function cddar  (#\D #\D #\A))
(define-c*r-function cdddr  (#\D #\D #\D))
(define-c*r-function caaaar (#\A #\A #\A #\A))
(define-c*r-function caaadr (#\A #\A #\A #\D))
(define-c*r-function caadar (#\A #\A #\D #\A))
(define-c*r-function caaddr (#\A #\A #\D #\D))
(define-c*r-function cadaar (#\A #\D #\A #\A))
(define-c*r-function cadadr (#\A #\D #\A #\D))
(define-c*r-function caddar (#\A #\D #\D #\A))
(define-c*r-function cadddr (#\A #\D #\D #\D))
(define-c*r-function cdaaar (#\D #\A #\A #\A))
(define-c*r-function cdaadr (#\D #\A #\A #\D))
(define-c*r-function cdadar (#\D #\A #\D #\A))
(define-c*r-function cdaddr (#\D #\A #\D #\D))
(define-c*r-function cddaar (#\D #\D #\A #\A))
(define-c*r-function cddadr (#\D #\D #\A #\D))
(define-c*r-function cdddar (#\D #\D #\D #\A))
(define-c*r-function cddddr (#\D #\D #\D #\D))

(define-c*r-function first   (#\A))
(define-c*r-function second  (#\A #\D))
(define-c*r-function third   (#\A #\D #\D))
(define-c*r-function fourth  (#\A #\D #\D #\D))
(define-c*r-function fifth   (#\A #\D #\D #\D #\D))
(define-c*r-function sixth   (#\A #\D #\D #\D #\D #\D))
(define-c*r-function seventh (#\A #\D #\D #\D #\D #\D #\D))
(define-c*r-function eighth  (#\A #\D #\D #\D #\D #\D #\D #\D))
(define-c*r-function ninth   (#\A #\D #\D #\D #\D #\D #\D #\D #\D))
(define-c*r-function tenth   (#\A #\D #\D #\D #\D #\D #\D #\D #\D #\D))

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defmacro define-setf-c*r-expander (function-name letters)
;;     (flet ((primitive (letter)
;;              (if (eql letter #\A) 'car 'cdr)))
;;       (flet ((one-iteration (string i original-tree)
;;                `(unless (consp remaining)
;; 		  (error 'setf-c*r-must-be-cons
;; 			 :datum remaining
;; 			 :access-string ,(subseq string i)
;; 			 :original-tree ,original-tree
;; 			 :name ',function-name))))
;; 	`(defsetf ,function-name (list) (new-object)
;; 	   `(let ((remaining ,list)
;; 		  (original ,list))
;; 	      ,@',(loop for i downfrom (length letters)
;; 			collect (one-iteration letters i 'original)
;; 			until (= i 1)
;; 			collect `(setf remaining (,(primitive (aref letters (1- i)))
;; 							      remaining)))
;; 	      (setf (,',(primitive (aref letters 0)) remaining) ,new-object)))))))

;; ;;; We get warnings when defining a setf expander if there
;; ;;; is already a setf function defined, so we start by 
;; ;;; undefining all the setf functions.

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (loop for name in '(caar cadr cdar cddr
;; 		      caaar caadr cadar caddr cdaar cdadr cddar cdddr
;; 		      caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
;; 		      cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
;; 		      first second third fourth fifth
;; 		      sixth seventh eighth ninth tenth)
;; 	do (fmakunbound (list 'setf name))))

;; (define-setf-c*r-expander caar "AA")
;; (define-setf-c*r-expander cadr "AD")
;; (define-setf-c*r-expander cdar "DA")
;; (define-setf-c*r-expander cddr "DD")
;; (define-setf-c*r-expander caaar "AAA")
;; (define-setf-c*r-expander caadr "AAD")
;; (define-setf-c*r-expander cadar "ADA")
;; (define-setf-c*r-expander caddr "ADD")
;; (define-setf-c*r-expander cdaar "DAA")
;; (define-setf-c*r-expander cdadr "DAD")
;; (define-setf-c*r-expander cddar "DDA")
;; (define-setf-c*r-expander cdddr "DDD")
;; (define-setf-c*r-expander caaaar "AAAA")
;; (define-setf-c*r-expander caaadr "AAAD")
;; (define-setf-c*r-expander caadar "AADA")
;; (define-setf-c*r-expander caaddr "AADD")
;; (define-setf-c*r-expander cadaar "ADAA")
;; (define-setf-c*r-expander cadadr "ADAD")
;; (define-setf-c*r-expander caddar "ADDA")
;; (define-setf-c*r-expander cadddr "ADDD")
;; (define-setf-c*r-expander cdaaar "DAAA")
;; (define-setf-c*r-expander cdaadr "DAAD")
;; (define-setf-c*r-expander cdadar "DADA")
;; (define-setf-c*r-expander cdaddr "DADD")
;; (define-setf-c*r-expander cddaar "DDAA")
;; (define-setf-c*r-expander cddadr "DDAD")
;; (define-setf-c*r-expander cdddar "DDDA")
;; (define-setf-c*r-expander cddddr "DDDD")

;; (define-setf-c*r-expander first   "A")
;; (define-setf-c*r-expander second  "AD")
;; (define-setf-c*r-expander third   "ADD")
;; (define-setf-c*r-expander fourth  "ADDD")
;; (define-setf-c*r-expander fifth   "ADDDD")
;; (define-setf-c*r-expander sixth   "ADDDDD")
;; (define-setf-c*r-expander seventh "ADDDDDD")
;; (define-setf-c*r-expander eighth  "ADDDDDDD")
;; (define-setf-c*r-expander ninth   "ADDDDDDDD")
;; (define-setf-c*r-expander tenth   "ADDDDDDDDD")

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
  (setf (car cons)
	new-value))

(defun (setf second) (new-value cons)
  (setf (first (rest cons))
	new-value))

(defun (setf third) (new-value cons)
  (setf (first (rest (rest cons)))
	new-value))

(defun (setf fourth) (new-value cons)
  (setf (first (rest (rest (rest cons))))
	new-value))

(defun (setf fifth) (new-value cons)
  (setf (first (rest (rest (rest (rest cons)))))
	new-value))

(defun (setf sixth) (new-value cons)
  (setf (first (rest (rest (rest (rest (rest cons))))))
	new-value))

(defun (setf seventh) (new-value cons)
  (setf (first (rest (rest (rest (rest (rest (rest cons)))))))
	new-value))

(defun (setf eighth) (new-value cons)
  (setf (first (rest (rest (rest (rest (rest (rest (rest cons))))))))
	new-value))

(defun (setf ninth) (new-value cons)
  (setf (first (rest (rest (rest (rest (rest (rest (rest (rest cons)))))))))
	new-value))

(defun (setf tenth) (new-value cons)
  (setf (first (rest (rest (rest (rest (rest (rest (rest (rest (rest cons))))))))))
	new-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function rest

(defun rest (list)
  (cond ((consp list)
	 (cdr list))
	((null list)
	 nil)
	(t
	 (error 'must-be-list
		:datum list
		:name 'rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Setf expander and setf function for rest

(eval-when (:compile-toplevel :load-toplevel :execute)
  (fmakunbound '(setf rest)))

(defsetf rest (list) (new-value)
  `(if (consp ,list)
       (progn (rplacd ,list ,new-value) ,new-value)
       (error 'must-be-cons
	      :datum ,list
	      :name '(setf rest))))

(defun (setf rest) (new-value list)
  (if (consp list)
      (progn (rplacd list new-value) new-value)
      (error 'must-be-cons
	     :datum list
	     :name '(setf rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; type list

(deftype list () '(or cons null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function list*

;;; this implementation assumes that there is no 
;;; structure sharing between the &rest argument
;;; and the last argument to apply
(defun list* (&rest elements)
  (when (null elements)
    (error 'at-least-one-argument-required :name 'list*))
  (if (null (cdr elements))
      (car elements)
      (loop for remaining on elements
	    until (null (cddr remaining))
	    finally (setf (cdr remaining)
			  (cadr remaining))
                    (return elements))))

(define-compiler-macro list* (&whole form &rest args)
  (cond  ((null args) form)
	 ((null (cdr args)) (car args))
	 (t `(cons ,(car args) (list* ,@(cdr args))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function last

(defun last (list &optional (n 1))
  (unless (typep list 'list)
    (error 'must-be-list
	   :datum list
	   :name 'last))
  (unless (typep n '(integer 0))
    (error 'must-be-nonnegative-integer
	   :datum n
	   :name 'last))
  (let ((remaining list))
    (loop repeat n
	  until (atom remaining)
	  do (setf remaining (cdr remaining)))
    (loop until (atom remaining)
	  do (setf list (cdr list))
	  do (setf remaining (cdr remaining)))
    list))

;;; special version of last used when the second argument to
;;; last is 1. 
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun last-1 (list)
    (unless (typep list 'list)
      (error 'must-be-list
	     :datum list
	     :name 'last))
    ;; We can use for ... on, because it uses atom to test for
    ;; the end of the list. 
    (loop for rest on list
	  do (setf list rest))
    list))

(define-compiler-macro last (&whole form list &optional (n 1))
  (if (eql n 1)
      `(last-1 ,list)
      form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function copy-list

(defun copy-list (list)
  (unless (typep list 'list)
    (error 'must-be-list
	   :datum list
	   :name 'copy-list))
  (if (null list)
      nil
      (let* ((result (list (car list)))
	     (trailer result))
	(loop do (setf list (cdr list))
	      until (atom list)
	      ;; list is not an atom, allocate a new cell
	      ;; at the end of the result list
	      do (setf (cdr trailer) (list (car list)))
	      do (setf trailer (cdr trailer)))
	;; when we come here, list is an atom,
	;; either NIL because the list was a proper list
	;; or some other atom because it was not a proper list
	(setf (cdr trailer) list)
	result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function list-length

;;; The standard requires the argument to be either a circular list
;;; or a proper list.  In case of a circular list, NIL should be 
;;; returned.  
(defun list-length (list)
  (cond ((consp list)
	 ;; The list starts with a cons.  We use the method by which
	 ;; one pointer (fast) advences by two steps in each
	 ;; iteration, and another pointer (slow) advances by one
	 ;; step.  If the two meet we have a circular list.  The only
	 ;; purpose of slow is to detect circularity.  The length of
	 ;; the list is determined by the number of cons cells seen by
	 ;; fast.
	 (let ((slow list)
	       (fast (cdr list))
	       ;; This variable counts the number of cons cells as
	       ;; seen by fast.
	       (length 1))
	   (loop do (when (eq slow fast)
		      ;; We have a circular list
		      (return-from list-length nil))
		    ;; Start by checking whether fast is a cons,
		    ;; because this is the most likely case, so will
		    ;; likely succeed.
		    (cond ((consp fast)
			   ;; We are not at the end of the list
			   ;; try to advance by cddr, but do it in
			   ;; two steps so that we can check that
			   ;; we are allowed to do that.
			   (setf fast (cdr fast))
			   ;; We now need to remember that the value of
			   ;; length is one less that the number of cons
			   ;; cells seen by fast. 
			   (cond ((consp fast)
				  ;; We indeed had two conses, so we can
				  ;; make fast advance by one more cons.
				  ;; This is the normal iteration case.
				  (setf fast (cdr fast))
				  ;; We advanced fast by cddr, so now we need to
				  ;; advance slow by cdr. 
				  (setf slow (cdr slow))
				  ;; The length is counted by the
				  ;; position of fast, so since we
				  ;; advanced by cdr, the length
				  ;; increases by 2. 
				  (incf length 2))
				 ((null fast)
				  ;; We reached the end of a proper
				  ;; list.  But we already advanced
				  ;; fast by cdr without incrementing
				  ;; length, so we need to compensate
				  ;; for that.
				  (return-from list-length (1+ length)))
				 (t
				  ;; The variable fast contains
				  ;; neither a a cons cell nor nil.
				  ;; This is a violation of the
				  ;; requirement that the list given
				  ;; should be either proper or
				  ;; circular.
				  (error 'must-be-proper-or-circular-list
					 :datum list
					 :name 'list-length))))
			  ((null fast)
			   ;; We reached the end of the list.
			   (return-from list-length length))
			  (t
			   ;; The variable fast contains
			   ;; neither a a cons cell nor nil.
			   ;; This is a violation of the
			   ;; requirement that the list given
			   ;; should be either proper or
			   ;; circular.
			   (error 'must-be-proper-or-circular-list
				  :datum list
				  :name 'list-length))))))
	((null list)
	 ;; The initial list was empty, and such a list has zero length
	 0)
	(t
	 ;; The argument given is neither a cons cell nor nil.  This
	 ;; is a violation of the requirement that the list given
	 ;; should be either proper or circular.
	 (error 'must-be-proper-or-circular-list
		:datum list
		:name 'list-length))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function make-list

(defun make-list (length &key (initial-element nil))
  (unless (typep length '(integer 0))
    (error 'must-be-nonnegative-integer
	   :datum length
	   :name 'make-list))
  (loop repeat length
	collect initial-element))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nthcdr

(defun nthcdr (n list)
  (unless (typep n '(integer 0))
    (error 'must-be-nonnegative-integer
	   :datum n
	   :name 'nthcdr))
  (loop until (zerop n)
	until (atom list)
	do (decf n)
	do (setf list (cdr list)))
  (when (and (plusp n) (not (null list)))
    (error 'must-be-cons
	   :datum list
	   :name 'nthcdr))
  list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nth

(defun nth (n list)
  (unless (typep n '(integer 0))
    (error 'must-be-nonnegative-integer
	   :datum n
	   :name 'nthcdr))
  (loop until (zerop n)
	until (atom list)
	do (decf n)
	do (setf list (cdr list)))
  (when (not (listp list))
    (error 'must-be-list
	   :datum list
	   :name 'nth))
  (car list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Setf expander and setf function for nth

(eval-when (:compile-toplevel :load-toplevel :execute)
  (fmakunbound '(setf nth)))

(defsetf nth (n list) (object)
  `(progn  (unless (typep ,n '(integer 0))
	     (error 'must-be-nonnegative-integer
		    :datum ,n
		    :name '(setf nth)))
	   (loop with remaining = ,list
		 with count = 0
		 until (atom remaining)
		 until (= count ,n)
		 do (setf remaining (cdr remaining))
		    (incf count)
		 finally  (when (not (consp remaining))
			    (error 'setf-nth-must-be-cons
				   :datum remaining
				   :name 'nth
				   :original-tree ,list
				   :cons-cell-count count))
			  (setf (car remaining) ,object))
	   ,object))

(defun (setf nth) (object n list)
  (unless (typep n '(integer 0))
    (error 'must-be-nonnegative-integer
	   :datum n
	   :name '(setf nth)))
  (loop with remaining = list
	with count = 0
	until (atom remaining)
	until (= count n)
	do (setf remaining (cdr remaining))
	   (incf count)
	finally  (when (not (consp remaining))
		   (error 'setf-nth-must-be-cons
			  :datum remaining
			  :name 'nth
			  :original-tree list
			  :cons-cell-count count))
		 (setf (car remaining) object))
  object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function copy-tree

;;; This can probably be done better by using iteration in one
;;; dimension and recurstion in the other.  Assuming trees are 
;;; typically wider than they are deep, it would be better to
;;; use iteration on the CDR and recursion on the CAR.

(defun copy-tree (tree)
  (if (atom tree)
      tree
      (cons (copy-tree (car tree)) (copy-tree (cdr tree)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function tree-equal 

(defun |tree-equal test=eq| (tree1 tree2)
  (or (and (atom tree1)
           (atom tree2)
           (eq tree1 tree2))
      (and (consp tree1)
           (consp tree2)
           (|tree-equal test=eq| (car tree1) (car tree2))
           (|tree-equal test=eq| (cdr tree1) (cdr tree2)))))

(defun |tree-equal test=eql| (tree1 tree2)
  (or (and (atom tree1)
           (atom tree2)
           (eql tree1 tree2))
      (and (consp tree1)
           (consp tree2)
           (|tree-equal test=eql| (car tree1) (car tree2))
           (|tree-equal test=eql| (cdr tree1) (cdr tree2)))))

(defun |tree-equal test-not=eq| (tree1 tree2)
  (or (and (atom tree1)
           (atom tree2)
           (not (eq tree1 tree2)))
      (and (consp tree1)
           (consp tree2)
           (|tree-equal test-not=eq| (car tree1) (car tree2))
           (|tree-equal test-not=eq| (cdr tree1) (cdr tree2)))))

(defun |tree-equal test-not=eql| (tree1 tree2)
  (or (and (atom tree1)
           (atom tree2)
           (not (eql tree1 tree2)))
      (and (consp tree1)
           (consp tree2)
           (|tree-equal test-not=eql| (car tree1) (car tree2))
           (|tree-equal test-not=eql| (cdr tree1) (cdr tree2)))))

(defun |tree-equal test=other| (tree1 tree2 test)
  (or (and (atom tree1)
           (atom tree2)
           (funcall test tree1 tree2))
      (and (consp tree1)
           (consp tree2)
           (|tree-equal test=other| (car tree1) (car tree2) test)
           (|tree-equal test=other| (cdr tree1) (cdr tree2) test))))

(defun |tree-equal test-not=other| (tree1 tree2 test)
  (or (and (atom tree1)
           (atom tree2)
           (not (funcall test tree1 tree2)))
      (and (consp tree1)
           (consp tree2)
           (|tree-equal test-not=other| (car tree1) (car tree2) test)
           (|tree-equal test-not=other| (cdr tree1) (cdr tree2) test))))

(defun tree-equal (tree1 tree2
                   &key
                   (test nil testp)
                   (test-not nil test-not-p))
  (when (and testp test-not-p)
    (error 'both-test-and-test-not-given
	   :name 'tree-equal))
  (if testp
      (if (eq test #'eq)
          (|tree-equal test=eq| tree1 tree2)
          (if (eq test #'eql)
              (|tree-equal test=eql| tree1 tree2)
              (|tree-equal test=other| tree1 tree2 test)))
      (if test-not-p 
          (if (eq test-not #'eq)
              (|tree-equal test-not=eq| tree1 tree2)
              (if (eq test-not #'eql)
                  (|tree-equal test-not=eql| tree1 tree2)
                  (|tree-equal test-not=other| tree1 tree2 test-not)))
          (|tree-equal test=eql| tree1 tree2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function endp

(defun endp (list)
  (unless (typep list 'list)
    (error 'must-be-list
	   :datum list
	   :name 'endp))
  (null list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiler macro for mapcar.  
;;;
;;; The function is defined earlier because it is needed in
;;; macroexpanders for push and pop.

;;; The compiler macro for mapcar generates code to loop
;;; individually over each list given, and thus avoids having
;;; a list of lists (which implies consing).  Since the number
;;; of lists given is known, we can use funcall instead of 
;;; apply when we call the function. 
(define-compiler-macro mapcar (&whole form function &rest lists)
  (if (null lists)
      form
      (let ((funvar (gensym))
	    (listvars (loop for var in lists collect (gensym)))
	    (vars (loop for var in lists collect (gensym))))
	`(loop with ,funvar = ,function
	       ,@(apply #'append (loop for listvar in listvars
				       for list in lists
				       collect `(with ,listvar = ,list)))
	       ,@(apply #'append (loop for var in vars
				       for listvar in listvars
				       collect `(for ,var = ,listvar then (cdr ,var))))
	       ,@(apply #'append (loop for var in vars
				       collect `(until (atom ,var))))
	       collect (funcall ,funvar ,@(loop for var in vars
						collect `(car ,var)))
	       finally (progn ,@(loop for var in vars
				      for listvar in listvars
				      collect `(unless (listp ,var)
						 (error 'must-be-proper-list
							:datum ,listvar
							:name 'mapcar))))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function mapc

(defun mapc (function &rest lists)
  (when (null lists)
    (error 'at-least-one-list-required :name 'mapc))
  (loop for remaining = lists
	  then (loop for list in remaining collect (cdr list))
	until (loop for list in remaining thereis (atom list))
	do (apply function
		  (loop for list in remaining collect (car list)))
	finally (loop for rem in remaining
		      for list in lists
		      do (when (not (listp rem))
			   (error 'must-be-proper-list
				  :datum list
				  :name 'mapc))))
  ;; The mapc function returns the first list
  (car lists))

;;; The compiler macro for mapc generates code to loop
;;; individually over each list given, and thus avoids having
;;; a list of lists (which implies consing).  Since the number
;;; of lists given is known, we can use funcall instead of 
;;; apply when we call the function. 
(define-compiler-macro mapc (&whole form function &rest lists)
  (if (null lists)
      form
      (let ((funvar (gensym))
	    (listvars (loop for var in lists collect (gensym)))
	    (vars (loop for var in lists collect (gensym))))
	`(loop with ,funvar = ,function
	       ,@(apply #'append (loop for listvar in listvars
				       for list in lists
				       collect `(with ,listvar = ,list)))
	       ,@(apply #'append (loop for var in vars
				       for listvar in listvars
				       collect `(for ,var = ,listvar then (cdr ,var))))
	       ,@(apply #'append (loop for var in vars
				       collect `(until (atom ,var))))
	       do (funcall ,funvar ,@(loop for var in vars
					   collect `(car ,var)))
	       finally (progn ,@(loop for var in vars
				      for listvar in listvars
				      collect `(unless (listp ,var)
						 (error 'must-be-proper-list
							:datum ,listvar
							:name 'mapc)))
			      (return ,(car listvars)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function maplist

(defun maplist (function &rest lists)
  (when (null lists)
    (error 'at-least-one-list-required :name 'maplist))
  (loop for remaining = lists
	  then (loop for list in remaining collect (cdr list))
	until (loop for list in remaining thereis (atom list))
	collect (apply function remaining)
	finally (loop for rem in remaining
		      for list in lists
		      do (when (not (listp rem))
			   (error 'must-be-proper-list
				  :datum list
				  :name 'maplist)))))

;;; The compiler macro for maplist generates code to loop
;;; individually over each list given, and thus avoids having
;;; a list of lists (which implies consing).  Since the number
;;; of lists given is known, we can use funcall instead of 
;;; apply when we call the function. 
(define-compiler-macro maplist (&whole form function &rest lists)
  (if (null lists)
      form
      (let ((funvar (gensym))
	    (listvars (loop for var in lists collect (gensym)))
	    (vars (loop for var in lists collect (gensym))))
	`(loop with ,funvar = ,function
	       ,@(apply #'append (loop for listvar in listvars
				       for list in lists
				       collect `(with ,listvar = ,list)))
	       ,@(apply #'append (loop for var in vars
				       for listvar in listvars
				       collect `(for ,var = ,listvar then (cdr ,var))))
	       ,@(apply #'append (loop for var in vars
				       collect `(until (atom ,var))))
	       collect (funcall ,funvar ,@vars)
	       finally (progn ,@(loop for var in vars
				      for listvar in listvars
				      collect `(unless (listp ,var)
						 (error 'must-be-proper-list
							:datum ,listvar
							:name 'maplist))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function mapl

(defun mapl (function &rest lists)
  (when (null lists)
    (error 'at-least-one-list-required :name 'mapl))
  (loop for remaining = lists
	  then (loop for list in remaining collect (cdr list))
	until (loop for list in remaining thereis (atom list))
	do (apply function remaining)
	finally (loop for rem in remaining
		      for list in lists
		      do (when (not (listp rem))
			   (error 'must-be-proper-list
				  :datum list
				  :name 'mapl))))
  ;; The mapl function returns the first list
  (car lists))

;;; The compiler macro for mapl generates code to loop
;;; individually over each list given, and thus avoids having
;;; a list of lists (which implies consing).  Since the number
;;; of lists given is known, we can use funcall instead of 
;;; apply when we call the function. 
(define-compiler-macro mapl (&whole form function &rest lists)
  (if (null lists)
      form
      (let ((funvar (gensym))
	    (listvars (loop for var in lists collect (gensym)))
	    (vars (loop for var in lists collect (gensym))))
	`(loop with ,funvar = ,function
	       ,@(apply #'append (loop for listvar in listvars
				       for list in lists
				       collect `(with ,listvar = ,list)))
	       ,@(apply #'append (loop for var in vars
				       for listvar in listvars
				       collect `(for ,var = ,listvar then (cdr ,var))))
	       ,@(apply #'append (loop for var in vars
				       collect `(until (atom ,var))))
	       do (funcall ,funvar ,@vars)
	       finally (progn ,@(loop for var in vars
				      for listvar in listvars
				      collect `(unless (listp ,var)
						 (error 'must-be-proper-list
							:datum ,listvar
							:name 'mapl)))
			      (return ,(car listvars)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function mapcan

(defun mapcan (function &rest lists)
  (when (null lists)
    (error 'at-least-one-list-required :name 'mapcan))
  (loop for remaining = lists
	  then (loop for list in remaining collect (cdr list))
	until (loop for list in remaining thereis (atom list))
	nconc (apply function
		     (loop for list in remaining collect (car list)))
	finally (loop for rem in remaining
		      for list in lists
		      do (when (not (listp rem))
			   (error 'must-be-proper-list
				  :datum list
				  :name 'mapcan)))))

;;; The compiler macro for mapcan generates code to loop
;;; individually over each list given, and thus avoids having
;;; a list of lists (which implies consing).  Since the number
;;; of lists given is known, we can use funcall instead of 
;;; apply when we call the function. 
(define-compiler-macro mapcan (&whole form function &rest lists)
  (if (null lists)
      form
      (let ((funvar (gensym))
	    (listvars (loop for var in lists collect (gensym)))
	    (vars (loop for var in lists collect (gensym))))
	`(loop with ,funvar = ,function
	       ,@(apply #'append (loop for listvar in listvars
				       for list in lists
				       collect `(with ,listvar = ,list)))
	       ,@(apply #'append (loop for var in vars
				       for listvar in listvars
				       collect `(for ,var = ,listvar then (cdr ,var))))
	       ,@(apply #'append (loop for var in vars
				       collect `(until (atom ,var))))
	       nconc (funcall ,funvar ,@(loop for var in vars
					      collect `(car ,var)))
	       finally (progn ,@(loop for var in vars
				      for listvar in listvars
				      collect `(unless (listp ,var)
						 (error 'must-be-proper-list
							:datum ,listvar
							:name 'mapcan))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function mapcon

(defun mapcon (function &rest lists)
  (when (null lists)
    (error 'at-least-one-list-required :name 'mapcon))
  (loop for remaining = lists
	  then (loop for list in remaining collect (cdr list))
	until (loop for list in remaining thereis (atom list))
	nconc (apply function remaining)
	finally (loop for rem in remaining
		      for list in lists
		      do (when (not (listp rem))
			   (error 'must-be-proper-list
				  :datum list
				  :name 'mapcon)))))

;;; The compiler macro for mapcon generates code to loop
;;; individually over each list given, and thus avoids having
;;; a list of lists (which implies consing).  Since the number
;;; of lists given is known, we can use funcall instead of 
;;; apply when we call the function. 
(define-compiler-macro mapcon (&whole form function &rest lists)
  (if (null lists)
      form
      (let ((funvar (gensym))
	    (listvars (loop for var in lists collect (gensym)))
	    (vars (loop for var in lists collect (gensym))))
	`(loop with ,funvar = ,function
	       ,@(apply #'append (loop for listvar in listvars
				       for list in lists
				       collect `(with ,listvar = ,list)))
	       ,@(apply #'append (loop for var in vars
				       for listvar in listvars
				       collect `(for ,var = ,listvar then (cdr ,var))))
	       ,@(apply #'append (loop for var in vars
				       collect `(until (atom ,var))))
	       nconc (funcall ,funvar ,@vars)
	       finally (progn ,@(loop for var in vars
				      for listvar in listvars
				      collect `(unless (listp ,var)
						 (error 'must-be-proper-list
							:datum ,listvar
							:name 'mapcon))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function revappend

(defun revappend (list tail)
  (loop with result = tail
	for remaining = list then (cdr remaining)
	until (atom remaining)
        do (push (car remaining) result)
        finally (unless (null remaining)
		  (error 'must-be-proper-list
			 :datum list
			 :name 'revappend))
		(return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nreconc

(defun nreconc (list tail)
  (loop with remaining = list
        with result = tail
        until (atom remaining)
        do (let ((temp (cdr remaining)))
             (setf (cdr remaining) result
                   result remaining
                   remaining temp))
        finally (unless (null remaining)
		  (error 'must-be-proper-list
			 :datum list
			 :name 'nreconc))
		(return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function butlast

;;; The HyperSpec doesn't specify what happens if n is 0.
;;; There is a note that says:
;;;
;;;     (butlast list n) ==  (ldiff list (last list n))
;;;
;;; but notes are not normative.  If you do believe this note,
;;; then (butlast '(1 2 3 . 4) 0) should return (1 2 3), i.e., 
;;; it doesn't return the unmodified list.  
;;;
;;; This implementation works according to the HyperSpec note
;;; when n is 0.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun butlast (list &optional (n 1))
    (unless (typep list 'list)
      (error 'must-be-list
	     :datum list
	     :name 'butlast))
    (unless (typep n '(integer 0))
      (error 'must-be-nonnegative-integer
	     :datum n
	     :name 'butlast))
    (let ((remaining list))
      (loop repeat n
	    until (atom remaining)
	    do (setf remaining (cdr remaining)))
      (loop for slow on remaining
	    until (atom remaining)
	    do (setf remaining (cdr remaining))
	    collect (prog1 (car list) (setf list (cdr list)))
	    until (atom remaining)
	    do (setf remaining (cdr remaining))
	    collect (prog1 (car list) (setf list (cdr list)))
	    do (when (eq slow remaining)
		 ;; we have a circular list
		 (error 'must-be-proper-or-dotted-list
			:datum list
			:name 'butlast))))))

;;; There is probably no point in making a special version of 
;;; butlast for n = 1, because the time is going to be dominated
;;; by consing up the new list anyawy. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nbutlast

;;; Special version for n = 1.  This version avoids that 
;;; the list be traversed several times, and since list traversal
;;; dominates the time here, this is a win.

(defun nbutlast-1 (list)
  (unless (typep list 'list)
    (error 'must-be-list
	   :datum list
	   :name 'nbutlast))
  (if (atom (cdr list))
      nil
      ;; The compiler probably isn't smart enough to eliminate
      ;; common subexpressions such as (cdr x), so we do it
      ;; manually by only doing a single cdr in each iteration,
      ;; and by storing pointers to the result in local variables.
      (let ((a list)
            (b (cdr list))
            (c (cddr list)))
        (loop for slow on list
	      until (atom c)
              do (setf a b
                       b c
                       c (cdr c))
	      until (atom c)
              do (setf a b
                       b c
                       c (cdr c))
	      do (when (eq slow c)
		   ;; we have a circular list
		   (error 'must-be-proper-or-dotted-list
			  :datum list
			  :name 'nbutlast)))
	(setf (cdr a) nil)
        list)))

(defun nbutlast (list &optional (n 1))
  (unless (typep list 'list)
    (error 'must-be-list
	   :datum list
	   :name 'nbutlast))
  (unless (typep n '(integer 0))
    (error 'must-be-nonnegative-integer
	   :datum n
	   :name 'nbutlast))
  (if (= n 1)
      (nbutlast-1 list)
      (let ((length (loop for slow on list
			  with conses = list
                          until (atom conses)
                          count t
			  do (setf conses (cdr conses))
                          until (atom conses)
                          count t
			  do (setf conses (cdr conses))
			  do (when (eq slow conses)
			       ;; we have a circular list
			       (error 'must-be-proper-or-dotted-list
				      :datum list
				      :name 'nbutlast)))))
        (if (<= length n)
            nil
            (progn (setf (cdr (nthcdr (- length (1+ n)) list)) nil)
                   list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function subst

(defun |subst key=identity test=eq| (new old tree)
  (labels ((traverse (tree)
             (cond ((eq tree old) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

(defun |subst key=identity test=eql| (new old tree)
  (labels ((traverse (tree)
             (cond ((eql tree old) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

(defun |subst key=other test=eq| (new old tree key)
  (labels ((traverse (tree)
             (cond ((eq (funcall key tree) old) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

(defun |subst key=other test=eql| (new old tree key)
  (labels ((traverse (tree)
             (cond ((eql (funcall key tree) old) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

(defun |subst key=identity test=other| (new old tree test)
  (labels ((traverse (tree)
             (cond ((funcall test old tree) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

(defun |subst key=other-test-other| (new old tree test key)
  (labels ((traverse (tree)
             (cond ((funcall test old (funcall key tree)) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

;;; We do not provide special versions for when the key is identity and
;;; the test-not is either eq or eql.  Here is why:
;;;
;;; Let's start with eq, and let's suppose the old item is an atom.
;;; Then either the old item is eq to the tree or not eq to the tree.
;;; If the old item is eq to the tree, then the tree too is an atom,
;;; and it is returned, because no substitutions are made, and we
;;; reached a leaf of the tree.  If the old item is not eq to the
;;; tree, then we make a substitution and return the new item.  In
;;; other words, this operation is the same as:
;;; (if (eq old-item tree) old-item new-item)
;;;
;;; Now suppose the old item is not an atom, and thus a cons.  Then
;;; the old item is either eq to the tree or not.  If the old item is
;;; not eq to the tree, then we return the new item and stop.  If the
;;; old item is eq to the tree, consider the two immediate subtrees
;;; (car and cdr) of the tree.  If none of them is eq to the the old
;;; item, then we return a cons where the car and cdr both contain the
;;; new item.  Now let's say that the car of the tree is eq to the old
;;; item.  Then it is also eq to the tree itself (beacuase we are
;;; handling the case where the old item and the tree are eq).  Then
;;; we have a circular structure, and it is undefined (the Hyperspec
;;; actually doesn't say that, but the Notes on the relevant page
;;; suggest it) what happens.  It is therefore very unlikely that
;;; someone would use this special case, so we have decided not to
;;; include it.
;;;
;;; A similar argument can be made for eql. 

;;; While it is possible that a test-not of eq or eql could be useful
;;; in combination with a key argument that is not identity, we do not
;;; think that case is very common either, so we omit it too. 

(defun |subst key=identity test-not=other| (new old tree test)
  (labels ((traverse (tree)
             (cond ((not (funcall test old tree)) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

(defun |subst key=other test-not=other| (new old tree test key)
  (labels ((traverse (tree)
             (cond ((not (funcall test old (funcall key tree))) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

(defun subst (new old tree
	      &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'subst))
  (if key
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|subst key=other test=eq| new old tree key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|subst key=other test=eql| new old tree key)
                  (|subst key=other-test-other| new old tree test key)))
          (if test-not-given
	      (|subst key=other test-not=other| new old tree test-not key)
              (|subst key=other test=eql| new old tree key)))
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|subst key=identity test=eq| new old tree)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|subst key=identity test=eql| new old tree)
                  (|subst key=identity test=other| new old tree test)))
          (if test-not-given
	      (|subst key=identity test-not=other| new old tree test-not)
              (|subst key=identity test=eql| new old tree)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function subst-if

(defun |subst-if key=other=identity| (new predicate tree)
  ;; Define a local function so as to avoid passing the new and
  ;; predicate arguments to each recursive call.
  (labels ((traverse (tree)
             (cond ((funcall predicate tree) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

(defun |subst-if key=other| (new predicate tree key)
  ;; Define a local function so as to avoid passing the new and
  ;; predicate arguments to each recursive call.
  (labels ((traverse (tree)
             (cond ((funcall predicate (funcall key tree)) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

(defun subst-if (new predicate tree &key key)
  (if (null key)
      (|subst-if key=other=identity| new predicate tree)
      (|subst-if key=other| new predicate tree key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function subst-if-not

(defun |subst-if-not key=identity| (new predicate tree)
  ;; Define a local function so as to avoid passing the new and
  ;; predicate arguments to each recursive call.
  (labels ((traverse (tree)
             (cond ((not (funcall predicate tree)) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

(defun |subst-if-not key=other| (new predicate tree key)
  ;; Define a local function so as to avoid passing the new and
  ;; predicate arguments to each recursive call.
  (labels ((traverse (tree)
             (cond ((not (funcall predicate (funcall key tree))) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

(defun subst-if-not (new predicate tree &key key)
  (if (null key)
      (|subst-if-not key=identity| new predicate tree)
      (|subst-if-not key=other| new predicate tree key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nsubst

(defun |nsubst key=identity test=eq| (new old tree)
  (labels ((traverse (tree)
             (cond ((eq (car tree) old)
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((eq (cdr tree) old)
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((eq tree old) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

(defun |nsubst key=identity test=eql| (new old tree)
  (labels ((traverse (tree)
             (cond ((eql (car tree) old)
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((eql (cdr tree) old)
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((eql tree old) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

(defun |nsubst key=other test=eq| (new old tree key)
  (labels ((traverse (tree)
             (cond ((eq (funcall key (car tree)) old)
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((eq (funcall key (cdr tree)) old)
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((eq (funcall key tree) old) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

(defun |nsubst key=other test=eql| (new old tree key)
  (labels ((traverse (tree)
             (cond ((eql (funcall key (car tree)) old)
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((eql (funcall key (cdr tree)) old)
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((eql (funcall key tree) old) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

(defun |nsubst key=identity test=other| (new old tree test)
  (labels ((traverse (tree)
             (cond ((funcall test old (car tree))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((funcall test old (cdr tree))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((funcall test old tree) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

(defun |nsubst key=other-test-other| (new old tree test key)
  (labels ((traverse (tree)
             (cond ((funcall test old (funcall key (car tree)))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((funcall test old (funcall key (cdr tree)))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((funcall test old (funcall key tree)) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

;;; As with subst, we do not provide special versions for a :test-not
;;; of eq or eql.  See comment above for an explanation.

(defun |nsubst key=identity test-not=other| (new old tree test)
  (labels ((traverse (tree)
             (cond ((not (funcall test old (car tree)))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((not (funcall test old (cdr tree)))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((not (funcall test old tree)) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

(defun |nsubst key=other test-not=other| (new old tree test key)
  (labels ((traverse (tree)
             (cond ((not (funcall test old (funcall key (car tree))))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((not (funcall test old (funcall key (cdr tree))))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((not (funcall test old (funcall key tree))) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

(defun nsubst (new old tree
	       &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'nsubst))
  (if key
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|nsubst key=other test=eq| new old tree key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|nsubst key=other test=eql| new old tree key)
                  (|nsubst key=other-test-other|  new old tree test key)))
          (if test-not-given
	      (|nsubst key=other test-not=other|  new old tree test-not key)
              (|nsubst key=other test=eql| new old tree key)))
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|nsubst key=identity test=eq| new old tree)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|nsubst key=identity test=eql| new old tree)
                  (|nsubst key=identity test=other|  new old tree test)))
          (if test-not-given
	      (|nsubst key=identity test-not=other|  new old tree test-not)
              (|nsubst key=identity test=eql| new old tree)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nsubst-if

(defun |nsubst-if key=other=identity| (new predicate tree)
  ;; Define a local function so as to avoid passing the new and
  ;; predicate arguments to each recursive call.
  (labels ((traverse (tree)
             (cond ((funcall predicate (car tree))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((funcall predicate (cdr tree))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((funcall predicate tree) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

(defun |nsubst-if key=other| (new predicate tree key)
  ;; Define a local function so as to avoid passing the new and
  ;; predicate arguments to each recursive call.
  (labels ((traverse (tree)
             (cond ((funcall predicate (funcall key (car tree)))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((funcall predicate (funcall key (cdr tree)))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((funcall predicate (funcall key tree)) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

(defun nsubst-if (new predicate tree &key key)
  (if (null key)
      (|nsubst-if key=other=identity| new predicate tree)
      (|nsubst-if key=other| new predicate tree key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nsubst-if-not

(defun |nsubst-if-not key=identity| (new predicate tree)
  ;; Define a local function so as to avoid passing the new and
  ;; predicate arguments to each recursive call.
  (labels ((traverse (tree)
             (cond ((not (funcall predicate (car tree)))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((not (funcall predicate (cdr tree)))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((not (funcall predicate tree)) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

(defun |nsubst-if-not key=other| (new predicate tree key)
  ;; Define a local function so as to avoid passing the new and
  ;; predicate arguments to each recursive call.
  (labels ((traverse (tree)
             (cond ((not (funcall predicate (funcall key (car tree))))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((not (funcall predicate (funcall key (cdr tree))))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((not (funcall predicate (funcall key tree))) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

(defun nsubst-if-not (new predicate tree &key key)
  (if (null key)
      (|nsubst-if-not key=identity| new predicate tree)
      (|nsubst-if-not key=other| new predicate tree key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function assoc

(defmacro with-alist-elements ((element-var alist function-name) &body body)
  ;; We can use for ... on, because it uses atom to test the end
  ;; of the list
  (let ((remaining (gensym))
	(alist-var (gensym)))
    `(loop with ,alist-var = ,alist
	   for ,remaining on ,alist-var
	   do (let ((,element-var (car ,remaining)))
		(cond ((consp ,element-var)
		       ,@body)
		      ((null ,element-var)
		       nil)
		      (t
		       (error 'must-be-list
			      :datum ,element-var
			      :name ',function-name))))
	   finally (unless (null ,remaining)
		     (error 'must-be-association-list
			    :datum ,alist-var
			    :name ',function-name))
		   (return nil))))

(defun |assoc key=identity test=eq| (item alist)
  (with-alist-elements (element alist assoc)
    (when (eq item (car element))
      (return element))))

(defun |assoc key=identity test=eql| (item alist)
  (with-alist-elements (element alist assoc)
    (when (eql item (car element))
      (return element))))
        
(defun |assoc key=other test=eq| (item alist key)
  (with-alist-elements (element alist assoc)
    (when (eq item (funcall key (car element)))
      (return element))))
  
(defun |assoc key=other test=eql| (item alist key)
  (with-alist-elements (element alist assoc)
    (when (eql item (funcall key (car element)))
      (return element))))

(defun |assoc key=identity test=other| (item alist test)
  (with-alist-elements (element alist assoc)
    (when (funcall test item (car element))
      (return element))))

(defun |assoc key=other-test-other| (item alist test key)
  (with-alist-elements (element alist assoc)
    (when (funcall test item (funcall key (car element)))
      (return element))))
  
(defun |assoc key=identity test-not=eq| (item alist)
  (with-alist-elements (element alist assoc)
    (when (not (eq item (car element)))
      (return element))))

(defun |assoc key=identity test-not=eql| (item alist)
  (with-alist-elements (element alist assoc)
    (when (not (eql item (car element)))
      (return element))))
        
(defun |assoc key=other test-not=eq| (item alist key)
  (with-alist-elements (element alist assoc)
    (when (not (eq item (funcall key (car element))))
      (return element))))
  
(defun |assoc key=other test-not=eql| (item alist key)
  (with-alist-elements (element alist assoc)
    (when (not (eql item (funcall key (car element))))
      (return element))))

(defun |assoc key=identity test-not=other| (item alist test)
  (with-alist-elements (element alist assoc)
    (when (not (funcall test item (car element)))
      (return element))))

(defun |assoc key=other test-not=other| (item alist test key)
  (with-alist-elements (element alist assoc)
    (when (not (funcall test item (funcall key (car element))))
      (return element))))
  
(defun assoc (item alist
	      &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'assoc))
  (if key
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|assoc key=other test=eq| item alist key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|assoc key=other test=eql| item alist key)
                  (|assoc key=other-test-other| item alist test key)))
          (if test-not-given
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (|assoc key=other test-not=eq| item alist key)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (|assoc key=other test-not=eql| item alist key)
                      (|assoc key=other test-not=other| item alist test-not key)))
              (|assoc key=other test=eql| item alist key)))
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|assoc key=identity test=eq| item alist)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|assoc key=identity test=eql| item alist)
                  (|assoc key=identity test=other| item alist test)))
          (if test-not-given
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (|assoc key=identity test-not=eq| item alist)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (|assoc key=identity test-not=eql| item alist)
                      (|assoc key=identity test-not=other| item alist test-not)))
              (|assoc key=identity test=eql| item alist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function rassoc

(defun |rassoc key=identity test=eq| (item alist)
  (with-alist-elements (element alist rassoc)
    (when (eq item (cdr element))
      (return element))))

(defun |rassoc key=identity test=eql| (item alist)
  (with-alist-elements (element alist rassoc)
    (when (eql item (cdr element))
      (return element))))
        
(defun |rassoc key=other test=eq| (item alist key)
  (with-alist-elements (element alist rassoc)
    (when (eq item (funcall key (cdr element)))
      (return element))))
  
(defun |rassoc key=other test=eql| (item alist key)
  (with-alist-elements (element alist rassoc)
    (when (eql item (funcall key (cdr element)))
      (return element))))

(defun |rassoc key=identity test=other| (item alist test)
  (with-alist-elements (element alist rassoc)
    (when (funcall test item (cdr element))
      (return element))))

(defun |rassoc key=other-test-other| (item alist test key)
  (with-alist-elements (element alist rassoc)
    (when (funcall test item (funcall key (cdr element)))
      (return element))))
  
(defun |rassoc key=identity test-not=eq| (item alist)
  (with-alist-elements (element alist rassoc)
    (when (not (eq item (cdr element)))
      (return element))))

(defun |rassoc key=identity test-not=eql| (item alist)
  (with-alist-elements (element alist rassoc)
    (when (not (eql item (cdr element)))
      (return element))))
        
(defun |rassoc key=other test-not=eq| (item alist key)
  (with-alist-elements (element alist rassoc)
    (when (not (eq item (funcall key (cdr element))))
      (return element))))
  
(defun |rassoc key=other test-not=eql| (item alist key)
  (with-alist-elements (element alist rassoc)
    (when (not (eql item (funcall key (cdr element))))
      (return element))))

(defun |rassoc key=identity test-not=other| (item alist test)
  (with-alist-elements (element alist rassoc)
    (when (not (funcall test item (cdr element)))
      (return element))))

(defun |rassoc key=other test-not=other| (item alist test key)
  (with-alist-elements (element alist rassoc)
    (when (not (funcall test item (funcall key (cdr element))))
      (return element))))
  
(defun rassoc (item alist
	       &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'rassoc))
  (if key
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|rassoc key=other test=eq| item alist key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|rassoc key=other test=eql| item alist key)
                  (|rassoc key=other-test-other| item alist test key)))
          (if test-not-given
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (|rassoc key=other test-not=eq| item alist key)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (|rassoc key=other test-not=eql| item alist key)
                      (|rassoc key=other test-not=other| item alist test-not key)))
              (|rassoc key=other test=eql| item alist key)))
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|rassoc key=identity test=eq| item alist)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|rassoc key=identity test=eql| item alist)
                  (|rassoc key=identity test=other| item alist test)))
          (if test-not-given
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (|rassoc key=identity test-not=eq| item alist)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (|rassoc key=identity test-not=eql| item alist)
                      (|rassoc key=identity test-not=other| item alist test-not)))
              (|rassoc key=identity test=eql| item alist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function rassoc-if

(defun |rassoc-if key=other=identity| (predicate alist)
  (with-alist-elements (element alist rassoc-if)
    (when (funcall predicate (cdr element))
      (return element))))

(defun |rassoc-if key=other| (predicate alist key)
  (with-alist-elements (element alist rassoc-if)
    (when (funcall predicate (funcall key (cdr element)))
      (return element))))

(defun rassoc-if (predicate alist &key key)
  (if key
      (|rassoc-if key=other| predicate alist key)
      (|rassoc-if key=other=identity| predicate alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function rassoc-if-not

(defun |rassoc-if-not key=identity| (predicate alist)
  (with-alist-elements (element alist rassoc-if-not)
    (when (not (funcall predicate (cdr element)))
      (return element))))

(defun |rassoc-if-not key=other| (predicate alist key)
  (with-alist-elements (element alist rassoc-if-not)
    (when (not (funcall predicate (funcall key (cdr element))))
      (return element))))

(defun rassoc-if-not (predicate alist &key key)
  (if key
      (|rassoc-if-not key=other| predicate alist key)
      (|rassoc-if-not key=identity| predicate alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function sublis

(defun |sublis key=identity test=eq| (alist tree)
  (let ((substitution-p nil))
    (labels ((traverse (tree)
	       (let ((entry (with-alist-elements (element alist sublis)
			      (when (eq tree (car element))
				(return element)))))
		 (cond ((not (null entry))
			(setf substitution-p t)
			(cdr entry))
		       ((atom tree) tree)
		       (t (cons (traverse (car tree))
				(traverse (cdr tree))))))))
      (let ((new-tree (traverse tree)))
	(if substitution-p new-tree tree)))))
  
(defun |sublis key=identity test=eql| (alist tree)
  (let ((substitution-p nil))
    (labels ((traverse (tree)
	       (let ((entry (with-alist-elements (element alist sublis)
			      (when (eql tree (car element))
				(return element)))))
		 (cond ((not (null entry))
			(setf substitution-p t)
			(cdr entry))
		       ((atom tree) tree)
		       (t (cons (traverse (car tree))
				(traverse (cdr tree))))))))
      (let ((new-tree (traverse tree)))
	(if substitution-p new-tree tree)))))
  
(defun |sublis key=other test=eq| (alist tree key)
  (let ((substitution-p nil))
    (labels ((traverse (tree)
	       (let ((entry (with-alist-elements (element alist sublis)
			      (when (eq (funcall key tree) (car element))
				(return element)))))
		 (cond ((not (null entry))
			(setf substitution-p t)
			(cdr entry))
		       ((atom tree) tree)
		       (t (cons (traverse (car tree))
				(traverse (cdr tree))))))))
      (let ((new-tree (traverse tree)))
	(if substitution-p new-tree tree)))))
  
(defun |sublis key=other test=eql| (alist tree key)
  (let ((substitution-p nil))
    (labels ((traverse (tree)
	       (let ((entry (with-alist-elements (element alist sublis)
			      (when (eql (funcall key tree) (car element))
				(return element)))))
		 (cond ((not (null entry))
			(setf substitution-p t)
			(cdr entry))
		       ((atom tree) tree)
		       (t (cons (traverse (car tree))
				(traverse (cdr tree))))))))
      (let ((new-tree (traverse tree)))
	(if substitution-p new-tree tree)))))
  
(defun |sublis key=identity test=other| (alist tree test)
  (let ((substitution-p nil))
    (labels ((traverse (tree)
	       (let ((entry (with-alist-elements (element alist sublis)
			      (when (funcall test (car element) tree)
				(return element)))))
		 (cond ((not (null entry))
			(setf substitution-p t)
			(cdr entry))
		       ((atom tree) tree)
		       (t (cons (traverse (car tree))
				(traverse (cdr tree))))))))
      (let ((new-tree (traverse tree)))
	(if substitution-p new-tree tree)))))
  
(defun |sublis key=other-test-other| (alist tree test key)
  (let ((substitution-p nil))
    (labels ((traverse (tree)
	       (let ((entry (with-alist-elements (element alist sublis)
			      (when (funcall test (car element) (funcall key tree))
				(return element)))))
		 (cond ((not (null entry))
			(setf substitution-p t)
			(cdr entry))
		       ((atom tree) tree)
		       (t (cons (traverse (car tree))
				(traverse (cdr tree))))))))
      (let ((new-tree (traverse tree)))
	(if substitution-p new-tree tree)))))

(defun |sublis key=identity test-not=other| (alist tree test)
  (let ((substitution-p nil))
    (labels ((traverse (tree)
	       (let ((entry (with-alist-elements (element alist sublis)
			      (when (not (funcall test (car element) tree))
				(return element)))))
		 (cond ((not (null entry))
			(setf substitution-p t)
			(cdr entry))
		       ((atom tree) tree)
		       (t (cons (traverse (car tree))
				(traverse (cdr tree))))))))
      (let ((new-tree (traverse tree)))
	(if substitution-p new-tree tree)))))
  
(defun |sublis key=other test-not=other| (alist tree test key)
  (let ((substitution-p nil))
    (labels ((traverse (tree)
	       (let ((entry (with-alist-elements (element alist sublis)
			      (when (not (funcall test (car element) (funcall key tree)))
				(return element)))))
		 (cond ((not (null entry))
			(setf substitution-p t)
			(cdr entry))
		       ((atom tree) tree)
		       (t (cons (traverse (car tree))
				(traverse (cdr tree))))))))
      (let ((new-tree (traverse tree)))
	(if substitution-p new-tree tree)))))

(defun sublis (alist tree
	       &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'sublis))
  (if key
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|sublis key=other test=eq| alist tree key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|sublis key=other test=eql| alist tree key)
                  (|sublis key=other-test-other| alist tree test key)))
          (if test-not-given
	      (|sublis key=other test-not=other| alist tree test-not key)
              (|sublis key=other test=eql| alist tree key)))
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|sublis key=identity test=eq| alist tree)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|sublis key=identity test=eql| alist tree)
                  (|sublis key=identity test=other| alist tree test)))
          (if test-not-given
	      (|sublis key=identity test-not=other| alist tree test-not)
              (|sublis key=identity test=eql| alist tree)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nsublis

(defun |nsublis key=identity test=eq| (alist tree)
  (labels ((traverse (tree)
             (let ((entry (with-alist-elements (element alist nsublis)
			    (when (eq (car tree) (car element))
			      (return element)))))
               (cond ((not (null entry)) (setf (car tree) (cdr entry)))
                     ((atom (car tree)) nil)
                     (t (traverse (car tree)))))
	     (let ((entry (with-alist-elements (element alist nsublis)
			    (when (eq (cdr tree) (car element))
			      (return element)))))
               (cond ((not (null entry)) (setf (cdr tree) (cdr entry)))
                     ((atom (cdr tree)) nil)
                     (t (traverse (cdr tree)))))))
    (let ((entry (with-alist-elements (element alist nsublis)
			    (when (eq tree (car element))
			      (return element)))))
      (cond ((not (null entry)) (cdr entry))
	    ((atom tree) tree)
	    (t (traverse tree) tree)))))

(defun |nsublis key=identity test=eql| (alist tree)
  (labels ((traverse (tree)
             (let ((entry (with-alist-elements (element alist nsublis)
			    (when (eql (car tree) (car element))
			      (return element)))))
               (cond ((not (null entry)) (setf (car tree) (cdr entry)))
                     ((atom (car tree)) nil)
                     (t (traverse (car tree)))))
	     (let ((entry (with-alist-elements (element alist nsublis)
			    (when (eql (cdr tree) (car element))
			      (return element)))))
               (cond ((not (null entry)) (setf (cdr tree) (cdr entry)))
                     ((atom (cdr tree)) nil)
                     (t (traverse (cdr tree)))))))
    (let ((entry (with-alist-elements (element alist nsublis)
			    (when (eql tree (car element))
			      (return element)))))
      (cond ((not (null entry)) (cdr entry))
	    ((atom tree) tree)
	    (t (traverse tree) tree)))))

(defun |nsublis key=other test=eq| (alist tree key)
  (labels ((traverse (tree)
             (let ((entry (with-alist-elements (element alist nsublis)
			    (when (eq (funcall key (car tree)) (car element))
			      (return element)))))
               (cond ((not (null entry)) (setf (car tree) (cdr entry)))
                     ((atom (car tree)) nil)
                     (t (traverse (car tree)))))
	     (let ((entry (with-alist-elements (element alist nsublis)
			    (when (eq (funcall key (cdr tree)) (car element))
			      (return element)))))
               (cond ((not (null entry)) (setf (cdr tree) (cdr entry)))
                     ((atom (cdr tree)) nil)
                     (t (traverse (cdr tree)))))))
    (let ((entry (with-alist-elements (element alist nsublis)
		   (when (eq (funcall key tree) (car element))
		     (return element)))))
      (cond ((not (null entry)) (cdr entry))
	    ((atom tree) tree)
	    (t (traverse tree) tree)))))

(defun |nsublis key=other test=eql| (alist tree key)
  (labels ((traverse (tree)
             (let ((entry (with-alist-elements (element alist nsublis)
			    (when (eql (funcall key (car tree)) (car element))
			      (return element)))))
               (cond ((not (null entry)) (setf (car tree) (cdr entry)))
                     ((atom (car tree)) nil)
                     (t (traverse (car tree)))))
	     (let ((entry (with-alist-elements (element alist nsublis)
			    (when (eql (funcall key (cdr tree)) (car element))
			      (return element)))))
               (cond ((not (null entry)) (setf (cdr tree) (cdr entry)))
                     ((atom (cdr tree)) nil)
                     (t (traverse (cdr tree)))))))
    (let ((entry (with-alist-elements (element alist nsublis)
			    (when (eql (funcall key tree) (car element))
			      (return element)))))
      (cond ((not (null entry)) (cdr entry))
	    ((atom tree) tree)
	    (t (traverse tree) tree)))))

(defun |nsublis key=identity test=other| (alist tree test)
  (labels ((traverse (tree)
             (let ((entry (with-alist-elements (element alist nsublis)
			    (when (funcall test (car element) (car tree))
			      (return element)))))
               (cond ((not (null entry)) (setf (car tree) (cdr entry)))
                     ((atom (car tree)) nil)
                     (t (traverse (car tree)))))
	     (let ((entry (with-alist-elements (element alist nsublis)
			    (when (funcall test (car element) (cdr tree))
			      (return element)))))
               (cond ((not (null entry)) (setf (cdr tree) (cdr entry)))
                     ((atom (cdr tree)) nil)
                     (t (traverse (cdr tree)))))))
    (let ((entry (with-alist-elements (element alist nsublis)
			    (when (funcall test (car element) tree)
			      (return element)))))
      (cond ((not (null entry)) (cdr entry))
	    ((atom tree) tree)
	    (t (traverse tree) tree)))))

(defun |nsublis key=other-test-other| (alist tree test key)
  (labels ((traverse (tree)
             (let ((entry (with-alist-elements (element alist nsublis)
			    (when (funcall test (car element) (funcall key (car tree)))
			      (return element)))))
               (cond ((not (null entry)) (setf (car tree) (cdr entry)))
                     ((atom (car tree)) nil)
                     (t (traverse (car tree)))))
	     (let ((entry (with-alist-elements (element alist nsublis)
			    (when (funcall test (car element) (funcall key (cdr tree)))
			      (return element)))))
               (cond ((not (null entry)) (setf (cdr tree) (cdr entry)))
                     ((atom (cdr tree)) nil)
                     (t (traverse (cdr tree)))))))
    (let ((entry (with-alist-elements (element alist nsublis)
			    (when (funcall test (car element) (funcall key tree))
			      (return element)))))
      (cond ((not (null entry)) (cdr entry))
	    ((atom tree) tree)
	    (t (traverse tree) tree)))))

(defun |nsublis key=identity test-not=other| (alist tree test)
  (labels ((traverse (tree)
             (let ((entry (with-alist-elements (element alist nsublis)
			    (when (not (funcall test (car element) (car tree)))
			      (return element)))))
               (cond ((not (null entry)) (setf (car tree) (cdr entry)))
                     ((atom (car tree)) nil)
                     (t (traverse (car tree)))))
	     (let ((entry (with-alist-elements (element alist nsublis)
			    (when (not (funcall test (car element) (cdr tree)))
			      (return element)))))
               (cond ((not (null entry)) (setf (cdr tree) (cdr entry)))
                     ((atom (cdr tree)) nil)
                     (t (traverse (cdr tree)))))))
    (let ((entry (with-alist-elements (element alist nsublis)
			    (when (not (funcall test (car element) tree))
			      (return element)))))
      (cond ((not (null entry)) (cdr entry))
	    ((atom tree) tree)
	    (t (traverse tree) tree)))))

(defun |nsublis key=other test-not=other| (alist tree test key)
  (labels ((traverse (tree)
             (let ((entry (with-alist-elements (element alist nsublis)
			    (when (not (funcall test (car element) (funcall key (car tree))))
			      (return element)))))
               (cond ((not (null entry)) (setf (car tree) (cdr entry)))
                     ((atom (car tree)) nil)
                     (t (traverse (car tree)))))
	     (let ((entry (with-alist-elements (element alist nsublis)
			    (when (not (funcall test (car element) (funcall key (cdr tree))))
			      (return element)))))
               (cond ((not (null entry)) (setf (cdr tree) (cdr entry)))
                     ((atom (cdr tree)) nil)
                     (t (traverse (cdr tree)))))))
    (let ((entry (with-alist-elements (element alist nsublis)
			    (when (not (funcall test (car element) (funcall key tree)))
			      (return element)))))
      (cond ((not (null entry)) (cdr entry))
	    ((atom tree) tree)
	    (t (traverse tree) tree)))))

(defun nsublis (alist tree
		&key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'nsublis))
  (if key
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|nsublis key=other test=eq| alist tree key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|nsublis key=other test=eql| alist tree key)
                  (|nsublis key=other-test-other| alist tree test key)))
          (if test-not-given
	      (|nsublis key=other test-not=other| alist tree test-not key)
              (|nsublis key=other test=eql| alist tree key)))
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|nsublis key=identity test=eq| alist tree)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|nsublis key=identity test=eql| alist tree)
                  (|nsublis key=identity test=other| alist tree test)))
          (if test-not-given
	      (|nsublis key=identity test-not=other| alist tree test-not)
              (|nsublis key=identity test=eql| alist tree)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Some helper macros

(defmacro with-proper-list-rests ((rest-var list function-name) &body body)
  (let ((list-var (gensym)))
    `(loop with ,list-var = ,list
	   for ,rest-var on ,list-var
	   do ,@body
	   finally (unless (null ,rest-var)
		     (error 'must-be-proper-list
			    :datum ,list-var
			    :name ,function-name)))))

(defmacro with-proper-list-elements ((element-var list function-name) &body body)
  (let ((list-var (gensym))
	(rest-var (gensym)))
    `(loop with ,list-var = ,list
	   for ,rest-var on ,list-var
	   for ,element-var = (car ,rest-var)
	   do ,@body
	   finally (unless (null ,rest-var)
		     (error 'must-be-proper-list
			    :datum ,list-var
			    :name ,function-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function member

(defun |member test=eq key=identity| (name item list)
  (with-proper-list-rests (rest list name)
    (when (eq item (car rest))
      (return rest))))

(defun |member test=eq key=other| (name item list key)
  (with-proper-list-rests (rest list name)
    (when (eq item (funcall key (car rest)))
      (return rest))))

(defun |member test-not=eq key=identity| (name item list)
  (with-proper-list-rests (rest list name)
    (when (not (eq item (car rest)))
      (return rest))))

(defun |member test-not=eq key=other| (name item list key)
  (with-proper-list-rests (rest list name)
    (when (not (eq item (funcall key (car rest))))
      (return rest))))

(defun |member test=eql key=identity| (name item list)
  (with-proper-list-rests (rest list name)
    (when (eql item (car rest))
      (return rest))))

(defun |member test=eql key=other| (name item list key)
  (with-proper-list-rests (rest list name)
    (when (eql item (funcall key (car rest)))
      (return rest))))

(defun |member test-not=eql key=identity| (name item list)
  (with-proper-list-rests (rest list name)
    (when (not (eql item (car rest)))
      (return rest))))

(defun |member test-not=eql key=other| (name item list key)
  (with-proper-list-rests (rest list name)
    (when (not (eql item (funcall key (car rest))))
      (return rest))))

(defun |member test=other key=identity| (name item list test)
  (with-proper-list-rests (rest list name)
    (when (funcall test item (car rest))
      (return rest))))

(defun |member test=other key=other| (name item list test key)
  (with-proper-list-rests (rest list name)
    (when (funcall test item (funcall key (car rest)))
      (return rest))))

(defun |member test-not=other key=identity| (name item list test)
  (with-proper-list-rests (rest list name)
    (when (not (funcall test item (car rest)))
      (return rest))))

(defun |member test-not=other key=other| (name item list test key)
  (with-proper-list-rests (rest list name)
    (when (not (funcall test item (funcall key (car rest))))
      (return rest))))

(defun member (item list
	       &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'member))
  (if key
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|member test=eq key=other| 'member item list key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|member test=eql key=other| 'member item list key)
                  (|member test=other key=other| 'member item list test key)))
          (if test-not-given
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (|member test-not=eq key=other| 'member item list key)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (|member test-not=eql key=other| 'member item list key)
                      (|member test-not=other key=other| 'member item list test-not key)))
              (|member test=eql key=other| 'member item list key)))
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|member test=eq key=identity| 'member item list)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|member test=eql key=identity| 'member item list)
                  (|member test=other key=identity| 'member item list test)))
          (if test-not-given
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (|member test-not=eq key=identity| 'member item list)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (|member test-not=eql key=identity| 'member item list)
                      (|member test-not=other key=identity| 'member item list test-not)))
              (|member test=eql key=identity| 'member item list)))))

;;; Special versions of the member function with the arguments
;;; of the test reversed.

(defun |member reversed test=other key=identity| (name item list test)
  (with-proper-list-rests (rest list name)
    (when (funcall test (car rest) item)
      (return rest))))

(defun |member reversed test=other key=other| (name item list test key)
  (with-proper-list-rests (rest list name)
    (when (funcall test (funcall key (car rest)) item)
      (return rest))))

(defun |member reversed test-not=other key=identity| (name item list test)
  (with-proper-list-rests (rest list name)
    (when (not (funcall test (car rest) item))
      (return rest))))

(defun |member reversed test-not=other key=other| (name item list test key)
  (with-proper-list-rests (rest list name)
    (when (not (funcall test (funcall key (car rest)) item))
      (return rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function member-if

(defun |member-if key=identity| (predicate list)
  (with-proper-list-rests (rest list 'member-if)
    (when (funcall predicate (car rest))
      (return rest))))

(defun |member-if key=other| (predicate list key)
  (with-proper-list-rests (rest list 'member-if)
    (when (funcall predicate (funcall key (car rest)))
      (return rest))))

(defun member-if (predicate list &key key)
  (if key
      (|member-if key=other| predicate list key)
      (|member-if key=identity| predicate list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function member-if-not

(defun |member-if-not key=identity| (predicate list)
  (with-proper-list-rests (rest list 'member-if-not)
    (unless (funcall predicate (car rest))
      (return rest))))

(defun |member-if-not key=other| (predicate list key)
  (with-proper-list-rests (rest list 'member-if-not)
    (unless (funcall predicate (funcall key (car rest)))
      (return rest))))

(defun member-if-not (predicate list &key key)
  (if key
      (|member-if-not key=other| predicate list key)
      (|member-if-not key=identity| predicate list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function acons

(defun acons (key datum alist)
  (cons (cons key datum) alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function pairlis

;;; The hyperspec says the consequences are undefined if the two
;;; lists do not have the same length.  We check for this situation
;;; and signal an error in that case. 

(defun pairlis (keys data &optional alist)
  (loop with result = alist
	with remaining-keys = keys
	with remaining-data = data
	until (or (atom remaining-keys) (atom remaining-data))
	do (push (cons (pop remaining-keys) (pop remaining-data)) result)
	finally (unless (and (null remaining-keys) (null remaining-data))
		  (cond ((and (atom remaining-keys) (not (null remaining-keys)))
			 (error 'must-be-proper-list
				:datum keys
				:name 'pairlis))
			((and (atom remaining-data) (not (null remaining-data)))
			 (error 'must-be-proper-list
				:datum data
				:name 'pairlis))
			(t
			 (error 'lists-must-have-the-same-length
				:list1 keys
				:list2 data
				:name 'pairlis))))
		(return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function copy-alist

;;; The HyperSpec says that the argument is an alist, and 
;;; in those cases, whenever the type doesn't correspond, it
;;; is legitimate to signal an error.  However, for copy-alist,
;;; the HyperSpec also says that any object that is referred to
;;; directly or indirectly is still shared betwee the argument
;;; and the resulting list, which suggests that any element that
;;; is not a cons should just be included in the resulting
;;; list as well.  And that is what we are doing. 

;;; We use (loop for ... on ...) because it uses ATOM to test for
;;; the end of the list.  Then we check that the atom is really 
;;; null, and if not, signal a type error.

(defun copy-alist (alist)
  (loop for remaining on alist
	collect (if (consp (car remaining))
		    (cons (caar remaining) (cdar remaining))
		    (car remaining))
	finally (unless (null remaining)
		  (error 'must-be-proper-list
			 :datum alist
			 :name 'copy-alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function tailp

;;; We could use (loop for ... on ...) here for consistency. 

(defun tailp (object list)
  (loop for rest = list then (cdr rest)
	until (atom rest)
	when (eql object rest)
	  return t
	finally (return (eql object rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function ldiff

(defun ldiff (list object)
  (if (or (eql list object) (atom list))
      nil
      (let* ((result (list (car list)))
             (current result)
             (remaining (cdr list)))
        (loop until (or (eql remaining object) (atom remaining))
              do (setf (cdr current) (list (car remaining)))
                 (setf current (cdr current))
                 (setf remaining (cdr remaining)))
        (if (eql remaining object)
            result
            (progn (setf (cdr current) remaining)
                   result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function union

(defun |union key=identity test=eql| (name list1 list2)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=eql key=identity| name element1 list2)
	(push element1 result)))
    result))

(defun |union key=identity test=eq| (name list1 list2)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=eq key=identity| name element1 list2)
	(push element1 result)))
    result))

(defun |union key=other test=eql| (name list1 list2 key)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=eql key=other| name (funcall key element1) list2 key)
	(push element1 result)))
    result))

(defun |union key=other test=eq| (name list1 list2 key)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=eq key=other| name (funcall key element1) list2 key)
	(push element1 result)))
    result))

(defun |union key=identity test=other| (name list1 list2 test)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=other key=identity| name element1 list2 test)
	(push element1 result)))
    result))

(defun |union key=other test=other| (name list1 list2 key test)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=other key=other| name (funcall key element1 ) list2 test key)
	(push element1 result)))
    result))

(defun |union key=identity test-not=other| (name list1 list2 test-not)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test-not=other key=identity| name element1 list2 test-not)
	(push element1 result)))
    result))

(defun |union key=other test-not=other| (name list1 list2 key test-not)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test-not=other key=other| name element1 list2 test-not key)
	(push element1 result)))
    result))

(defun |union key=identity test=eq hash| (name list1 list2)
  (let ((table (make-hash-table :test #'eq)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun |union key=identity test=eql hash| (name list1 list2)
  (let ((table (make-hash-table :test #'eql)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun |union key=identity test=equal hash| (name list1 list2)
  (let ((table (make-hash-table :test #'equal)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun |union key=identity test=equalp hash| (name list1 list2)
  (let ((table (make-hash-table :test #'equalp)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun |union key=other test=eq hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'eq)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun |union key=other test=eql hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'eql)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun |union key=other test=equal hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'equal)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun |union key=other test=equalp hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'equalp)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun union (list1 list2
	      &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'union))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|union key=other test=eq hash| 'union list1 list2 key)
		       (|union key=other test=eq| 'union list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|union key=other test=eql hash| 'union list1 list2 key)
		       (|union key=other test=eql| 'union list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|union key=other test=equal hash| 'union list1 list2 key)
		       (|union key=other test=other| 'union list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|union key=other test=equalp hash| 'union list1 list2 key)
		       (|union key=other test=other| 'union list1 list2 key #'equalp)))
		  (t
		   (|union key=other test=other| 'union list1 list2 key test)))
	    (if test-not-given
		(|union key=other test-not=other| 'union list1 list2 key test-not)
		(if use-hash
		    (|union key=other test=eql hash| 'union list1 list2 key)
		    (|union key=other test=eql| 'union list1 list2 key))))
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|union key=identity test=eq hash| 'union list1 list2)
		       (|union key=identity test=eq| 'union list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|union key=identity test=eql hash| 'union list1 list2)
		       (|union key=identity test=eql| 'union list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|union key=identity test=equal hash| 'union list1 list2)
		       (|union key=identity test=other| 'union list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|union key=identity test=equalp hash| 'union list1 list2)
		       (|union key=identity test=other| 'union list1 list2 #'equalp)))
		  (t
		   (|union key=identity test=other| 'union list1 list2 test)))
	    (if test-not-given
		(|union key=identity test-not=other| 'union list1 list2 test-not)
		(if use-hash
		    (|union key=identity test=eql hash| 'union list1 list2)
		    (|union key=identity test=eql| 'union list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nunion

;;; We take advantage of the fact that the standard doesn't 
;;; require this function to have any side effects. 

(defun nunion (list1 list2
	       &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'nunion))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|union key=other test=eq hash| 'nunion list1 list2 key)
		       (|union key=other test=eq| 'nunion list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|union key=other test=eql hash| 'nunion list1 list2 key)
		       (|union key=other test=eql| 'nunion list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|union key=other test=equal hash| 'nunion list1 list2 key)
		       (|union key=other test=other| 'nunion list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|union key=other test=equalp hash| 'nunion list1 list2 key)
		       (|union key=other test=other| 'nunion list1 list2 key #'equalp)))
		  (t
		   (|union key=other test=other| 'nunion list1 list2 key test)))
	    (if test-not-given
		(|union key=other test-not=other| 'nunion list1 list2 key test-not)
		(if use-hash
		    (|union key=other test=eql hash| 'nunion list1 list2 key)
		    (|union key=other test=eql| 'nunion list1 list2 key))))
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|union key=identity test=eq hash| 'nunion list1 list2)
		       (|union key=identity test=eq| 'nunion list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|union key=identity test=eql hash| 'nunion list1 list2)
		       (|union key=identity test=eql| 'nunion list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|union key=identity test=equal hash| 'nunion list1 list2)
		       (|union key=identity test=other| 'nunion list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|union key=identity test=equalp hash| 'nunion list1 list2)
		       (|union key=identity test=other| 'nunion list1 list2 #'equalp)))
		  (t
		   (|union key=identity test=other| 'nunion list1 list2 test)))
	    (if test-not-given
		(|union key=identity test-not=other| 'nunion list1 list2 test-not)
		(if use-hash
		    (|union key=identity test=eql hash| 'nunion list1 list2)
		    (|union key=identity test=eql| 'nunion list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function intersection

(defun |intersection key=identity test=eql| (name list1 list2)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (when (|member test=eql key=identity| name element1 list2)
	(push element1 result)))
    result))

(defun |intersection key=identity test=eq| (name list1 list2)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (when (|member test=eq key=identity| name element1 list2)
	(push element1 result)))
    result))

(defun |intersection key=other test=eql| (name list1 list2 key)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (when (|member test=eql key=other| name (funcall key element1) list2 key)
	(push element1 result)))
    result))

(defun |intersection key=other test=eq| (name list1 list2 key)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (when (|member test=eq key=other| name (funcall key element1) list2 key)
	(push element1 result)))
    result))

(defun |intersection key=identity test=other| (name list1 list2 test)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (when (|member test=other key=identity| name element1 list2 test)
	(push element1 result)))
    result))

(defun |intersection key=other test=other| (name list1 list2 key test)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (when (|member test=other key=other| name (funcall key element1) list2 test key)
	(push element1 result)))
    result))

(defun |intersection key=identity test-not=other| (name list1 list2 test-not)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (when (|member test-not=other key=identity| name element1 list2 test-not)
	(push element1 result)))
    result))

(defun |intersection key=other test-not=other| (name list1 list2 key test-not)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (when (|member test-not=other key=other| name (funcall key element1) list2 test-not key)
	(push element1 result)))
    result))

(defun |intersection key=identity test=eq hash| (name list1 list2)
  (let ((table (make-hash-table :test #'eq))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (when (gethash element table)
	(push element result)))
    result))

(defun |intersection key=identity test=eql hash| (name list1 list2)
  (let ((table (make-hash-table :test #'eql))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (when (gethash element table)
	(push element result)))
    result))

(defun |intersection key=identity test=equal hash| (name list1 list2)
  (let ((table (make-hash-table :test #'equal))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (when (gethash element table)
	(push element result)))
    result))

(defun |intersection key=identity test=equalp hash| (name list1 list2)
  (let ((table (make-hash-table :test #'equalp))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (when (gethash element table)
	(push element result)))
    result))

(defun |intersection key=other test=eq hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'eq))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (when (gethash (funcall key element) table)
	(push element result)))
    result))

(defun |intersection key=other test=eql hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'eql))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (when (gethash (funcall key element) table)
	(push element result)))
    result))

(defun |intersection key=other test=equal hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'equal))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (when (gethash (funcall key element) table)
	(push element result)))
    result))
	 
(defun |intersection key=other test=equalp hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'equalp))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (when (gethash (funcall key element) table)
	(push element result)))
    result))

(defun intersection (list1 list2
		     &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'intersection))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|intersection key=other test=eq hash| 'intersection list1 list2 key)
		       (|intersection key=other test=eq| 'intersection list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|intersection key=other test=eql hash| 'intersection list1 list2 key)
		       (|intersection key=other test=eql| 'intersection list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|intersection key=other test=equal hash| 'intersection list1 list2 key)
		       (|intersection key=other test=other| 'intersection list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|intersection key=other test=equalp hash| 'intersection list1 list2 key)
		       (|intersection key=other test=other| 'intersection list1 list2 key #'equalp)))
		  (t
		   (|intersection key=other test=other| 'intersection list1 list2 key test)))
	    (if test-not-given
		(|intersection key=other test-not=other| 'intersection list1 list2 key test-not)
		(if use-hash
		    (|intersection key=other test=eql hash| 'intersection list1 list2 key)
		    (|intersection key=other test=eql| 'intersection list1 list2 key))))
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|intersection key=identity test=eq hash| 'intersection list1 list2)
		       (|intersection key=identity test=eq| 'intersection list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|intersection key=identity test=eql hash| 'intersection list1 list2)
		       (|intersection key=identity test=eql| 'intersection list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|intersection key=identity test=equal hash| 'intersection list1 list2)
		       (|intersection key=identity test=other| 'intersection list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|intersection key=identity test=equalp hash| 'intersection list1 list2)
		       (|intersection key=identity test=other| 'intersection list1 list2 #'equalp)))
		  (t
		   (|intersection key=identity test=other| 'intersection list1 list2 test)))
	    (if test-not-given
		(|intersection key=identity test-not=other| 'intersection list1 list2 test-not)
		(if use-hash
		    (|intersection key=identity test=eql hash| 'intersection list1 list2)
		    (|intersection key=identity test=eql| 'intersection list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nintersection

;;; We take advantage of the fact that the standard doesn't 
;;; require this function to have any side effects. 

(defun nintersection (list1 list2
		      &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'nintersection))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|intersection key=other test=eq hash| 'nintersection list1 list2 key)
		       (|intersection key=other test=eq| 'nintersection list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|intersection key=other test=eql hash| 'nintersection list1 list2 key)
		       (|intersection key=other test=eql| 'nintersection list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|intersection key=other test=equal hash| 'nintersection list1 list2 key)
		       (|intersection key=other test=other| 'nintersection list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|intersection key=other test=equalp hash| 'nintersection list1 list2 key)
		       (|intersection key=other test=other| 'nintersection list1 list2 key #'equalp)))
		  (t
		   (|intersection key=other test=other| 'nintersection list1 list2 key test)))
	    (if test-not-given
		(|intersection key=other test-not=other| 'nintersection list1 list2 key test-not)
		(if use-hash
		    (|intersection key=other test=eql hash| 'nintersection list1 list2 key)
		    (|intersection key=other test=eql| 'nintersection list1 list2 key))))
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|intersection key=identity test=eq hash| 'nintersection list1 list2)
		       (|intersection key=identity test=eq| 'nintersection list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|intersection key=identity test=eql hash| 'nintersection list1 list2)
		       (|intersection key=identity test=eql| 'nintersection list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|intersection key=identity test=equal hash| 'nintersection list1 list2)
		       (|intersection key=identity test=other| 'nintersection list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|intersection key=identity test=equalp hash| 'nintersection list1 list2)
		       (|intersection key=identity test=other| 'nintersection list1 list2 #'equalp)))
		  (t
		   (|intersection key=identity test=other| 'nintersection list1 list2 test)))
	    (if test-not-given
		(|intersection key=identity test-not=other| 'nintersection list1 list2 test-not)
		(if use-hash
		    (|intersection key=identity test=eql hash| 'nintersection list1 list2)
		    (|intersection key=identity test=eql| 'nintersection list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function set-difference

(defun |set-difference key=identity test=eql| (name list1 list2)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=eql key=identity| name element list2)
	(push element result)))
    result))

(defun |set-difference key=identity test=eq| (name list1 list2)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=eq key=identity| name element list2)
	(push element result)))
    result))

(defun |set-difference key=other test=eql| (name list1 list2 key)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=eql key=other| name (funcall key element) list2 key)
	(push element result)))
    result))

(defun |set-difference key=other test=eq| (name list1 list2 key)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=eq key=other| name (funcall key element) list2 key)
	(push element result)))
    result))

(defun |set-difference key=identity test=other| (name list1 list2 test)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=other key=identity| name element list2  test)
	(push element result)))
    result))

(defun |set-difference key=other test=other| (name list1 list2 key test)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=other key=other| name (funcall key element) list2 test key)
	(push element result)))
    result))

(defun |set-difference key=identity test-not=other| (name list1 list2 test-not)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test-not=other key=identity| name element list2 test-not)
	(push element result)))
    result))

(defun |set-difference key=other test-not=other| (name list1 list2 key test-not)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test-not=other key=other| name (funcall key element) list2 test-not key)
	(push element result)))
    result))

(defun |set-difference key=identity test=eq hash| (name list1 list2)
  (let ((table (make-hash-table :test #'eq))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table)
	(push element result)))
    result))

(defun |set-difference key=identity test=eql hash| (name list1 list2)
  (let ((table (make-hash-table :test #'eql))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table)
	(push element result)))
    result))

(defun |set-difference key=identity test=equal hash| (name list1 list2)
  (let ((table (make-hash-table :test #'equal))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table)
	(push element result)))
    result))

(defun |set-difference key=identity test=equalp hash| (name list1 list2)
  (let ((table (make-hash-table :test #'equalp))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table)
	(push element result)))
    result))

(defun |set-difference key=other test=eq hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'eq))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table)
	(push element result)))
    result))

(defun |set-difference key=other test=eql hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'eql))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table)
	(push element result)))
    result))

(defun |set-difference key=other test=equal hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'equal))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table)
	(push element result)))
    result))

(defun |set-difference key=other test=equalp hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'equalp))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table)
	(push element result)))
    result))

(defun set-difference (list1 list2
		       &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'set-difference))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|set-difference key=other test=eq hash| 'set-difference list1 list2 key)
		       (|set-difference key=other test=eq| 'set-difference list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|set-difference key=other test=eql hash| 'set-difference list1 list2 key)
		       (|set-difference key=other test=eql| 'set-difference list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|set-difference key=other test=equal hash| 'set-difference list1 list2 key)
		       (|set-difference key=other test=other| 'set-difference list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|set-difference key=other test=equalp hash| 'set-difference list1 list2 key)
		       (|set-difference key=other test=other| 'set-difference list1 list2 key #'equalp)))
		  (t
		   (|set-difference key=other test=other| 'set-difference list1 list2 key test)))
	    (if test-not-given
		(|set-difference key=other test-not=other| 'set-difference list1 list2 key test-not)
		(if use-hash
		    (|set-difference key=other test=eql hash| 'set-difference list1 list2 key)
		    (|set-difference key=other test=eql| 'set-difference list1 list2 key))))
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|set-difference key=identity test=eq hash| 'set-difference list1 list2)
		       (|set-difference key=identity test=eq| 'set-difference list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|set-difference key=identity test=eql hash| 'set-difference list1 list2)
		       (|set-difference key=identity test=eql| 'set-difference list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|set-difference key=identity test=equal hash| 'set-difference list1 list2)
		       (|set-difference key=identity test=other| 'set-difference list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|set-difference key=identity test=equalp hash| 'set-difference list1 list2)
		       (|set-difference key=identity test=other| 'set-difference list1 list2 #'equalp)))
		  (t
		   (|set-difference key=identity test=other| 'set-difference list1 list2 test)))
	    (if test-not-given
		(|set-difference key=identity test-not=other| 'set-difference list1 list2 test-not)
		(if use-hash
		    (|set-difference key=identity test=eql hash| 'set-difference list1 list2)
		    (|set-difference key=identity test=eql| 'set-difference list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nset-difference

;;; We take advantage of the fact that the standard doesn't 
;;; require this function to have any side effects. 

(defun nset-difference (list1 list2
			&key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'nset-difference))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|set-difference key=other test=eq hash| 'nset-difference list1 list2 key)
		       (|set-difference key=other test=eq| 'nset-difference list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|set-difference key=other test=eql hash| 'nset-difference list1 list2 key)
		       (|set-difference key=other test=eql| 'nset-difference list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|set-difference key=other test=equal hash| 'nset-difference list1 list2 key)
		       (|set-difference key=other test=other| 'nset-difference list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|set-difference key=other test=equalp hash| 'nset-difference list1 list2 key)
		       (|set-difference key=other test=other| 'nset-difference list1 list2 key #'equalp)))
		  (t
		   (|set-difference key=other test=other| 'nset-difference list1 list2 key test)))
	    (if test-not-given
		(|set-difference key=other test-not=other| 'nset-difference list1 list2 key test-not)
		(if use-hash
		    (|set-difference key=other test=eql hash| 'nset-difference list1 list2 key)
		    (|set-difference key=other test=eql| 'nset-difference list1 list2 key))))
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|set-difference key=identity test=eq hash| 'nset-difference list1 list2)
		       (|set-difference key=identity test=eq| 'nset-difference list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|set-difference key=identity test=eql hash| 'nset-difference list1 list2)
		       (|set-difference key=identity test=eql| 'nset-difference list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|set-difference key=identity test=equal hash| 'nset-difference list1 list2)
		       (|set-difference key=identity test=other| 'nset-difference list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|set-difference key=identity test=equalp hash| 'nset-difference list1 list2)
		       (|set-difference key=identity test=other| 'nset-difference list1 list2 #'equalp)))
		  (t
		   (|set-difference key=identity test=other| 'nset-difference list1 list2 test)))
	    (if test-not-given
		(|set-difference key=identity test-not=other| 'nset-difference list1 list2 test-not)
		(if use-hash
		    (|set-difference key=identity test=eql hash| 'nset-difference list1 list2)
		    (|set-difference key=identity test=eql| 'nset-difference list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function adjoin

(defun |adjoin key=identity test=eq| (name item list)
  (if (|member test=eq key=identity| name item list)
      list
      (cons item list)))

(defun |adjoin key=identity test=eql| (name item list)
  (if (|member test=eql key=identity| name item list)
      list
      (cons item list)))

(defun |adjoin key=identity test=other| (name item list test)
  (if (|member test=other key=identity| name item list test)
      list
      (cons item list)))

(defun |adjoin key=identity test-not=other| (name item list test-not)
  (if (|member test-not=other key=identity| name item list test-not)
      list
      (cons item list)))

(defun |adjoin key=other test=eq| (name item list key)
  (if (|member test=eq key=other| name (funcall key item) list key)
      list
      (cons item list)))

(defun |adjoin key=other test=eql| (name item list key)
  (if (|member test=eql key=other| name (funcall key item) list key)
      list
      (cons item list)))

(defun |adjoin key=other test=other| (name item list key test)
  (if (|member test=other key=other| name (funcall key item) list test key)
      list
      (cons item list)))

(defun |adjoin key=other test-not=other| (name item list key test-not)
  (if (|member test-not=other key=other| name (funcall key item) list test-not key)
      list
      (cons item list)))

(defun adjoin (item list 
	       &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'adjoin))
  (if key
      (if test-given
	  (if (or (eq test #'eq) (eq test 'eq))
	      (|adjoin key=other test=eq| 'adjoin item list key)
	      (if (or (eq test #'eql) (eq test 'eql))
		  (|adjoin key=other test=eql| 'adjoin item list key)
		  (|adjoin key=other test=other| 'adjoin item list key test)))
	  (if test-not-given
	      (|adjoin key=other test-not=other| 'adjoin item list key test-not)
	      (|adjoin key=other test=eql| 'adjoin item list key)))
      (if test-given
	  (if (or (eq test #'eq) (eq test 'eq))
	      (|adjoin key=identity test=eq| 'adjoin item list)
	      (if (or (eq test #'eql) (eq test 'eql))
		  (|adjoin key=identity test=eql| 'adjoin item list)
		  (|adjoin key=identity test=other| 'adjoin item list test)))
	  (if test-not-given
	      (|adjoin key=identity test-not=other| 'adjoin item list test-not)
	      (|adjoin key=identity test=eql| 'adjoin item list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function set-exclusive-or

(defun |set-exclusive-or key=identity test=eql| (name list1 list2)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=eql key=identity| name element list2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (|member test=eql key=identity| name element list1)
	(push element result)))
    result))

(defun |set-exclusive-or key=identity test=eq| (name list1 list2)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=eq key=identity| name element list2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (|member test=eq key=identity| name  element list1)
	(push element result)))
    result))

(defun |set-exclusive-or key=other test=eql| (name list1 list2 key)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=eql key=other| name (funcall key element) list2 key)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (|member test=eql key=other| name (funcall key element) list1 key)
	(push element result)))
    result))

(defun |set-exclusive-or key=other test=eq| (name list1 list2 key)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=eq key=other| name (funcall key element) list2 key)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (|member test=eq key=other| name (funcall key element) list1 key)
	(push element result)))
    result))

(defun |set-exclusive-or key=identity test=other| (name list1 list2 test)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=other key=identity| name element1 list2 test)
	(push element1 result)))
    ;; we need to use a member with a test with reversed order or arguments
    (with-proper-list-elements (element2 list2 name)
      (unless (|member reversed test=other key=identity| name element2 list1 test)
	(push element2 result)))
    result))

(defun |set-exclusive-or key=other test=other| (name list1 list2 key test)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=other key=other| name (funcall key element1) list2 test key)
	(push element1 result)))
    ;; we need to use a member with a test with reversed order or arguments
    (with-proper-list-elements (element2 list2 name)
      (unless (|member reversed test=other key=other| name (funcall key element2) list1 test key)
	(push element2 result)))
    result))

(defun |set-exclusive-or key=identity test-not=other| (name list1 list2 test-not)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test-not=other key=identity| name element1 list2 test-not)
	(push element1 result)))
    ;; we need to use a member with a test with reversed order or arguments
    (with-proper-list-elements (element2 list2 name)
      (unless (|member reversed test-not=other key=identity| name element2 list1 test-not)
	(push element2 result)))
    result))

(defun |set-exclusive-or key=other test-not=other| (name list1 list2 key test-not)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test-not=other key=other| name (funcall key element1) list2 test-not key)
	(push element1 result)))
    ;; we need to use a member with a test with reversed order or arguments
    (with-proper-list-elements (element2 list2 name)
      (unless (|member reversed test-not=other key=other| name (funcall key element2) list1 test-not key)
	(push element2 result)))
    result))

(defun |set-exclusive-or key=identity test=eq hash| (name list1 list2)
  (let ((table1 (make-hash-table :test #'eq))
	(table2 (make-hash-table :test #'eq))
	(result '()))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table1) t))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table2) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (gethash element table1)
	(push element result)))
    result))

(defun |set-exclusive-or key=identity test=eql hash| (name list1 list2)
  (let ((table1 (make-hash-table :test #'eql))
	(table2 (make-hash-table :test #'eql))
	(result '()))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table1) t))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table2) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (gethash element table1)
	(push element result)))
    result))

(defun |set-exclusive-or key=identity test=equal hash| (name list1 list2)
  (let ((table1 (make-hash-table :test #'equal))
	(table2 (make-hash-table :test #'equal))
	(result '()))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table1) t))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table2) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (gethash element table1)
	(push element result)))
    result))

(defun |set-exclusive-or key=identity test=equalp hash| (name list1 list2)
  (let ((table1 (make-hash-table :test #'equalp))
	(table2 (make-hash-table :test #'equalp))
	(result '()))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table1) t))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table2) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (gethash element table1)
	(push element result)))
    result))

(defun |set-exclusive-or key=other test=eq hash| (name list1 list2 key)
  (let ((table1 (make-hash-table :test #'eq))
	(table2 (make-hash-table :test #'eq))
	(result '()))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table1) t))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table2) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (gethash (funcall key element) table1)
	(push element result)))
    result))

(defun |set-exclusive-or key=other test=eql hash| (name list1 list2 key)
  (let ((table1 (make-hash-table :test #'eql))
	(table2 (make-hash-table :test #'eql))
	(result '()))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table1) t))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table2) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (gethash (funcall key element) table1)
	(push element result)))
    result))

(defun |set-exclusive-or key=other test=equal hash| (name list1 list2 key)
  (let ((table1 (make-hash-table :test #'equal))
	(table2 (make-hash-table :test #'equal))
	(result '()))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table1) t))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table2) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (gethash (funcall key element) table1)
	(push element result)))
    result))

(defun |set-exclusive-or key=other test=equalp hash| (name list1 list2 key)
  (let ((table1 (make-hash-table :test #'equalp))
	(table2 (make-hash-table :test #'equalp))
	(result '()))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table1) t))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table2) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (gethash (funcall key element) table1)
	(push element result)))
    result))

(defun set-exclusive-or (list1 list2
			 &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'set-exclusive-or))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|set-exclusive-or key=other test=eq hash| 'set-exclusive-or list1 list2 key)
		       (|set-exclusive-or key=other test=eq| 'set-exclusive-or list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|set-exclusive-or key=other test=eql hash| 'set-exclusive-or list1 list2 key)
		       (|set-exclusive-or key=other test=eql| 'set-exclusive-or list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|set-exclusive-or key=other test=equal hash| 'set-exclusive-or list1 list2 key)
		       (|set-exclusive-or key=other test=other| 'set-exclusive-or list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|set-exclusive-or key=other test=equalp hash| 'set-exclusive-or list1 list2 key)
		       (|set-exclusive-or key=other test=other| 'set-exclusive-or list1 list2 key #'equalp)))
		  (t
		   (|set-exclusive-or key=other test=other| 'set-exclusive-or list1 list2 key test)))
	    (if test-not-given
		(|set-exclusive-or key=other test-not=other| 'set-exclusive-or list1 list2 key test-not)
		(if use-hash
		    (|set-exclusive-or key=other test=eql hash| 'set-exclusive-or list1 list2 key)
		    (|set-exclusive-or key=other test=eql| 'set-exclusive-or list1 list2 key))))
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|set-exclusive-or key=identity test=eq hash| 'set-exclusive-or list1 list2)
		       (|set-exclusive-or key=identity test=eq| 'set-exclusive-or list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|set-exclusive-or key=identity test=eql hash| 'set-exclusive-or list1 list2)
		       (|set-exclusive-or key=identity test=eql| 'set-exclusive-or list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|set-exclusive-or key=identity test=equal hash| 'set-exclusive-or list1 list2)
		       (|set-exclusive-or key=identity test=other| 'set-exclusive-or list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|set-exclusive-or key=identity test=equalp hash| 'set-exclusive-or list1 list2)
		       (|set-exclusive-or key=identity test=other| 'set-exclusive-or list1 list2 #'equalp)))
		  (t
		   (|set-exclusive-or key=identity test=other| 'set-exclusive-or list1 list2 test)))
	    (if test-not-given
		(|set-exclusive-or key=identity test-not=other| 'set-exclusive-or list1 list2 test-not)
		(if use-hash
		    (|set-exclusive-or key=identity test=eql hash| 'set-exclusive-or list1 list2)
		    (|set-exclusive-or key=identity test=eql| 'set-exclusive-or list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nset-exclusive-or

;;; We take advantage of the fact that the standard doesn't 
;;; require this function to have any side effects. 

(defun nset-exclusive-or (list1 list2
			  &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'nset-exclusive-or))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|set-exclusive-or key=other test=eq hash| 'nset-exclusive-or list1 list2 key)
		       (|set-exclusive-or key=other test=eq| 'nset-exclusive-or list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|set-exclusive-or key=other test=eql hash| 'nset-exclusive-or list1 list2 key)
		       (|set-exclusive-or key=other test=eql| 'nset-exclusive-or list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|set-exclusive-or key=other test=equal hash| 'nset-exclusive-or list1 list2 key)
		       (|set-exclusive-or key=other test=other| 'nset-exclusive-or list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|set-exclusive-or key=other test=equalp hash| 'nset-exclusive-or list1 list2 key)
		       (|set-exclusive-or key=other test=other| 'nset-exclusive-or list1 list2 key #'equalp)))
		  (t
		   (|set-exclusive-or key=other test=other| 'nset-exclusive-or list1 list2 key test)))
	    (if test-not-given
		(|set-exclusive-or key=other test-not=other| 'nset-exclusive-or list1 list2 key test-not)
		(if use-hash
		    (|set-exclusive-or key=other test=eql hash| 'nset-exclusive-or list1 list2 key)
		    (|set-exclusive-or key=other test=eql| 'nset-exclusive-or list1 list2 key))))
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|set-exclusive-or key=identity test=eq hash| 'nset-exclusive-or list1 list2)
		       (|set-exclusive-or key=identity test=eq| 'nset-exclusive-or list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|set-exclusive-or key=identity test=eql hash| 'nset-exclusive-or list1 list2)
		       (|set-exclusive-or key=identity test=eql| 'nset-exclusive-or list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|set-exclusive-or key=identity test=equal hash| 'nset-exclusive-or list1 list2)
		       (|set-exclusive-or key=identity test=other| 'nset-exclusive-or list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|set-exclusive-or key=identity test=equalp hash| 'nset-exclusive-or list1 list2)
		       (|set-exclusive-or key=identity test=other| 'nset-exclusive-or list1 list2 #'equalp)))
		  (t
		   (|set-exclusive-or key=identity test=other| 'nset-exclusive-or list1 list2 test)))
	    (if test-not-given
		(|set-exclusive-or key=identity test-not=other| 'nset-exclusive-or list1 list2 test-not)
		(if use-hash
		    (|set-exclusive-or key=identity test=eql hash| 'nset-exclusive-or list1 list2)
		    (|set-exclusive-or key=identity test=eql| 'nset-exclusive-or list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function subsetp

(defun |subsetp key=identity test=eql| (name list1 list2)
  (with-proper-list-elements (element list1 name)
    (unless (|member test=eql key=identity| name element list2)
      (return-from |subsetp key=identity test=eql| nil)))
  t)

(defun |subsetp key=identity test=eq| (name list1 list2)
  (with-proper-list-elements (element list1 name)
    (unless (|member test=eq key=identity| name element list2)
      (return-from |subsetp key=identity test=eq| nil)))
  t)

(defun |subsetp key=other test=eql| (name list1 list2 key)
  (with-proper-list-elements (element list1 name)
    (unless (|member test=eql key=other| name (funcall key element) list2 key)
      (return-from |subsetp key=other test=eql| nil)))
  t)

(defun |subsetp key=other test=eq| (name list1 list2 key)
  (with-proper-list-elements (element list1 name)
    (unless (|member test=eq key=other| name (funcall key element) list2 key)
      (return-from |subsetp key=other test=eq| nil)))
  t)

(defun |subsetp key=identity test=other| (name list1 list2 test)
  (with-proper-list-elements (element list1 name)
    (unless (|member test=other key=identity| name element list2 test)
      (return-from |subsetp key=identity test=other| nil)))
  t)

(defun |subsetp key=other test=other| (name list1 list2 key test)
  (with-proper-list-elements (element list1 name)
    (unless (|member test=other key=other| name (funcall key element) list2 test key)
      (return-from |subsetp key=other test=other| nil)))
  t)

(defun |subsetp key=identity test-not=other| (name list1 list2 test-not)
  (with-proper-list-elements (element list1 name)
    (unless (|member test-not=other key=identity| name  element list2 test-not)
      (return-from |subsetp key=identity test-not=other| nil)))
  t)

(defun |subsetp key=other test-not=other| (name list1 list2 key test-not)
  (with-proper-list-elements (element list1 name)
    (unless (|member test-not=other key=other| name (funcall key element) list2 test-not key)
      (return-from |subsetp key=other test-not=other| nil)))
  t)

(defun |subsetp key=identity test=eq hash| (name list1 list2)
  (let ((table (make-hash-table :test #'eq)))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table)
	(return-from |subsetp key=identity test=eq hash| nil))))
  t)

(defun |subsetp key=identity test=eql hash| (name list1 list2)
  (let ((table (make-hash-table :test #'eql)))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table)
	(return-from |subsetp key=identity test=eql hash| nil))))
  t)

(defun |subsetp key=identity test=equal hash| (name list1 list2)
  (let ((table (make-hash-table :test #'equal)))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table)
	(return-from |subsetp key=identity test=equal hash| nil))))
  t)

(defun |subsetp key=identity test=equalp hash| (name list1 list2)
  (let ((table (make-hash-table :test #'equalp)))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table)
	(return-from |subsetp key=identity test=equalp hash| nil))))
  t)

(defun |subsetp key=other test=eq hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'eq)))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table)
	(return-from |subsetp key=other test=eq hash| nil))))
  t)

(defun |subsetp key=other test=eql hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'eql)))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table)
	(return-from |subsetp key=other test=eql hash| nil))))
  t)

(defun |subsetp key=other test=equal hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'equal)))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table)
	(return-from |subsetp key=other test=equal hash| nil))))
  t)

(defun |subsetp key=other test=equalp hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'equalp)))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table)
	(return-from |subsetp key=other test=equalp hash| nil))))
  t)

(defun subsetp (list1 list2
		&key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'subsetp))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|subsetp key=other test=eq hash| 'subsetp list1 list2 key)
		       (|subsetp key=other test=eq| 'subsetp list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|subsetp key=other test=eql hash| 'subsetp list1 list2 key)
		       (|subsetp key=other test=eql| 'subsetp list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|subsetp key=other test=equal hash| 'subsetp list1 list2 key)
		       (|subsetp key=other test=other| 'subsetp list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|subsetp key=other test=equalp hash| 'subsetp list1 list2 key)
		       (|subsetp key=other test=other| 'subsetp list1 list2 key #'equalp)))
		  (t
		   (|subsetp key=other test=other| 'subsetp list1 list2 key test)))
	    (if test-not-given
		(|subsetp key=other test-not=other| 'subsetp list1 list2 key test-not)
		(if use-hash
		    (|subsetp key=other test=eql hash| 'subsetp list1 list2 key)
		    (|subsetp key=other test=eql| 'subsetp list1 list2 key))))
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|subsetp key=identity test=eq hash| 'subsetp list1 list2)
		       (|subsetp key=identity test=eq| 'subsetp list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|subsetp key=identity test=eql hash| 'subsetp list1 list2)
		       (|subsetp key=identity test=eql| 'subsetp list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|subsetp key=identity test=equal hash| 'subsetp list1 list2)
		       (|subsetp key=identity test=other| 'subsetp list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|subsetp key=identity test=equalp hash| 'subsetp list1 list2)
		       (|subsetp key=identity test=other| 'subsetp list1 list2 #'equalp)))
		  (t
		   (|subsetp key=identity test=other| 'subsetp list1 list2 test)))
	    (if test-not-given
		(|subsetp key=identity test-not=other| 'subsetp list1 list2 test-not)
		(if use-hash
		    (|subsetp key=identity test=eql hash| 'subsetp list1 list2)
		    (|subsetp key=identity test=eql| 'subsetp list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function getf

(defun getf (plist indicator &optional default)
  (unless (typep plist 'list)
    (error 'must-be-property-list
	   :datum plist
	   :name 'getf))
  (loop for rest on plist by #'cddr
	do (unless (consp (cdr rest))
	     (error 'must-be-property-list
		    :datum plist
		    'getf))
	when (eq (car rest) indicator)
	  return (cadr rest)
	finally (unless (null rest)
		  (error 'must-be-property-list
		    :datum plist
		    'getf))
		(return default)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Setf expander for getf

(define-setf-expander getf (place indicator &optional default &environment env)
  (let ((indicator-var (gensym))
	(default-var (gensym))
	(value-var (gensym)))
    (multiple-value-bind (vars vals store-vars writer-form reader-form)
	(get-setf-expansion place env)
      (values (append vars (list indicator-var default-var))
	      (append vals (list indicator default))
	      (list value-var)
	      `(let ((,default-var ,default-var))
		 (declare (ignore ,default-var))
		 (loop for rest on ,reader-form by #'cddr
		       when (eq (car rest) ,indicator-var)
			 do (setf (cadr rest) ,value-var)
			    (return nil)
		       finally (let ((,(car store-vars)
				      (list* ,indicator-var ,value-var ,reader-form)))
				 ,writer-form))
		 ,value-var)
	      `(getf ,reader-form ,indicator-var ,default)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function get-properties

(defun get-properties (plist indicator-list)
  (unless (typep plist 'list)
    (error 'must-be-property-list
	   :datum plist
	   :name 'getf))
  (loop for rest on plist by #'cddr
	do (unless (consp (cdr rest))
	     (error 'must-be-property-list
		    :datum plist
		    :name 'getf))
	   (let ((temp (member (car rest) indicator-list :test #'eq)))
	     (unless (null temp)
	       (return-from get-properties (values (car temp) (cadr rest) rest))))
	finally (unless (null rest)
		  (error 'must-be-property-list
			 :datum plist
			 :name 'getf))
		(return (values nil nil nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro remf

;;; FIXME, I guess macros such as remf should check that 
;;; store-vars has a length of 1.

(defmacro remf (place indicator &environment env)
  (let ((indicator-var (gensym)))
    (multiple-value-bind (vars vals store-vars writer-form reader-form)
	(get-setf-expansion place env)
      `(let (,@(mapcar #'list vars vals)
	     (,indicator-var ,indicator))
	 (let ((,(car store-vars) ,reader-form))
	   (if (null ,(car store-vars))
	       nil
	       (if (eq (car ,(car store-vars)) ,indicator-var)
		   (progn (setf ,(car store-vars) (cddr ,(car store-vars)))
			  ,writer-form
			  t)
		   (loop for rest on (cdr ,(car store-vars)) by #'cddr
			 when (null (cdr rest))
			   return nil
			 do (unless (consp (cddr rest))
			      (error 'must-be-property-list
				     :datum ,(car store-vars)
				     :name 'getf))
			 when (eq (cadr rest) ,indicator-var)
			   do (setf (cdr rest) (cdddr rest))
			      (return t)
			 finally (unless (null rest)
				   (error 'must-be-property-list
					  :datum ,(car store-vars)
					  :name 'getf))))))))))
