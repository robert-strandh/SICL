(in-package #:sicl-code-utilities)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lambda list utilities.

;;; There is much to say about lambda lists.  
;;; 
;;; There are 10 different types of lambda lists and they vary both
;;; with respect to syntax and semantics.  It gets pretty messy in
;;; fact.
;;;
;;; Lambda lists admit something known as "lambda list keywords".
;;; They are not keywords in the normal sense of the word (i.e.,
;;; symbols in the :KEYWORD package), but just ordinary symbols in
;;; the COMMON-LISP package that happen to have names that start with
;;; the `&' character (ampersand).
;;;
;;; The lambda list keywords that are allowed in each different type
;;; of lambda list are clearly indicated in the CLHS for that type of
;;; lambda list.  However, the CLHS also allows for
;;; implementation-specific lambda list keywords.  The complete list
;;; of lambda list keywords that a particular implementation
;;; recognizes is available as the value of the variable
;;; LAMBDA-LIST-KEYWORDS.  However, there is no way to determine what
;;; an implementation-specific lambda list keyword means, nor how it
;;; is used or even in which type of lambda list it is allowed.  There
;;; is also no indication as to whether implementation-specific lambda
;;; list keywords must begin with `&'.  
;;;
;;; The lambda list keywords that are recognized by the CLHS are:
;;; &allow-other-keys, &aux, &body, &environment, &key, &optional,
;;; &rest, and &whole.  The lambda list keywords &body and &rest are
;;; synonymous, but good style gives preference to one rather than the
;;; other according to the type of lambda list it occurs in.
;;;
;;; To make things more complicated, the CLHS does not tell us how to
;;; handle occurrences of a particular lambda list keyword in a lambda
;;; list of a type that does not recognize it.  One interpretation
;;; could be that such a lambda list keyword should be treated as just
;;; any symbol, so that it becomes the name of a parameter.  But with
;;; this interpretation, a program can have some subtle bugs just
;;; because a programmer incorrectly believes that a particular
;;; lambda-list keyword is acceptable in a type of lambda list where
;;; in fact it is not.  According to another interpretation, a
;;; program-error should be signaled in this case.  At the very least,
;;; it seems reasonable to give a style-warning in that case.
;;;
;;; Similarly, the CLHS does not indicate how to handle occurrences of
;;; symbols that do not occur in LAMBDA-LIST-KEYWORDS, but that happen
;;; to start with `&'.  Again, some subtle bugs could result if such a
;;; situation were not to be flagged to the programmer.  Again, at the
;;; very least, a style warning seems to be appropriate.
;;;
;;; Lambda list keywords have different arities, i.e., the number of
;;; items that can follow it in a lambda list.  The good news is that
;;; each lambda list keyword has the same arity no matter what type of
;;; lambda list it occurs in.  Thus &allow-other-keys always has arity
;;; 0 (zero), &rest, &body, &whole, and &environment always have arity
;;; 1 (one), and the remaining ones (&aux, &key, and &optional) can
;;; take any number of items, so have arbitrary arity.  
;;;
;;; Another piece of relatively good news is that the order in which
;;; lambda list keywords can occur in a lambda list is independent of
;;; the type of lambda list in which they occur, and that the relative
;;; order between two lambda list keywords is fixed, with &environment
;;; being the only exception, because it can occur anywhere (except
;;; before &whole) in the lambda lists in which it is allowed.
;;;
;;; A piece of not-so-good news is that &whole, whenever it is
;;; allowed, must appear first in the lambda list.  That is, not only
;;; first as in the first lambda list keyword, but as the first item
;;; in the lambda list, before the list of required variables.  This
;;; rule messes up syntax checking a bit.  

;;; A list of lambda list keywords in the order that they can occur in
;;; a lambda list (except &environment, which can occur anywhere, and
;;; except &rest and &body which do not have any relative order
;;; because they cannot both occur).  For each keyword, we indicate
;;; its min and max arity, where NIL means unbounded.
(defparameter *lambda-list-keywords*
  `((&whole 1 1)
    (&environment 1 1)
    (&optional 0 nil)
    (&rest 1 1)
    (&body 1 1)
    (&key 0 nil)
    (&allow-other-keys 0 0)
    (&aux 0 nil)))

(defun potential-lambda-list-keyword-p (object)
  (and (symbolp object)
       (plusp (length (symbol-name object)))
       (eql (char (symbol-name object) 0) #\&)))

;;; Use this function for lambda lists that can be proper or dotted.
(defun check-lambda-list-not-circular (lambda-list processor processee)
  (when (eq (nth-value 1 (list-structure lambda-list)) :circular)
    (error 'lambda-list-must-not-be-circular
	   :form lambda-list
	   :processor processor
	   :processee processee)))

;;; Use this function for lambda lists that must be proper lists.
(defun check-lambda-list-proper (lambda-list processor processee)
  (unless (eq (nth-value 1 (list-structure lambda-list)) :proper)
    (error 'lambda-list-must-be-proper-list
	   :form lambda-list
	   :processor processor
	   :processee processee)))

(defun check-lambda-list-keywords (lambda-list keywords processor processee)
  ;; We assume that KEYWORDS is a subset of LAMBDA-LIST-KEYWORDS, in
  ;; other words that we are given only valid lambda list keywords as
  ;; defined by the system.
  (let* (;; All symbols in the lambda list that look like they might
	 ;; be lambda-list keywords, in the order that the occur in
	 ;; the lambda list.  Multiple occurrences are preserved.
	 (potential (loop for remaining = lambda-list then (cdr remaining)
			  while (consp remaining)
			  when (potential-lambda-list-keyword-p (car remaining))
			    collect (car remaining)))

	 ;; All symbols in the lambda list that are also lambda-list
	 ;; keywords as defined by the system, in the order that they
	 ;; occur in the lambda list. 
	 (real (remove-if-not (lambda (x) (member x lambda-list-keywords))
			      potential))
	 ;; All symbols in the lambda list that look like they might
	 ;; be lambda-list keywords, but that are not lambda list
	 ;; keywords defined by the system, in any old order. 
	 (suspect (set-difference potential lambda-list-keywords))
	 ;; All symbols in the lambda list that are also lambda-list
	 ;; keywords as defined by the system, but that are not in the
	 ;; list of lambda list keywords allowed for this type of
	 ;; lambda list, in any old order.
	 (forbidden (set-difference real keywords))
	 ;; All symbols in the lambda list that are also in the list
	 ;; of valid keywords for this lambda list, in the order that
	 ;; they appear in the lambda list.  Multiple occurrences are
	 ;; preserved.
	 (to-process (remove-if-not (lambda (x) (member x keywords))
				    potential)))
    ;; Check for forbidden keywords.
    (unless (null forbidden)
	(error 'lambda-list-keyword-not-allowed
	       :form lambda-list
	       :keyword (car forbidden)
	       :processor processor
	       :processee processee))
    ;; Check for suspect keywords.
    (unless (null suspect)
      (warn 'suspect-lambda-list-keyword
	    :form lambda-list
	    :keyword (car suspect)
	    :processor processor
	    :processee processee))
    ;; Check for multiple occurrences.
    (loop for keyword in to-process
	  do (when (> (count keyword to-process) 1)
	       (error 'multiple-occurrences-of-lambda-list-keyword
		      :form lambda-list
		      :keyword keyword
		      :processor processor
		      :processee processee)))
    (when (> (+ (count '&body to-process) (count '&rest to-process)) 1)
      (error 'both-rest-and-body-occur-in-lambda-list
	     :form lambda-list
	     :processor processor
	     :processee processee))
    ;; Check the order of keywords.
    (loop for rem = to-process then (cdr rem)
	  until (null (cdr rem))
	  do (when (> (position (car rem) *lambda-list-keywords* :key #'car)
		      (position (cadr rem) *lambda-list-keywords* :key #'car))
	       (error 'incorrect-keyword-order
		      :form lambda-list
		      :keyword1 (car rem)
		      :keyword2 (cadr rem)
		      :processor processor
		      :processee processee)))
    ;; Check arities.
    (flet ((check-arity (keyword number-of-args)
	     (if (eq keyword '&whole)
		 (when (zerop number-of-args)
		   (error "arity zero not allowed"))
		 (let ((arities (cdr (assoc keyword *lambda-list-keywords*))))
		   (when (or (< number-of-args (car arities))
			     (and (not (null (cadr arities)))
				  (> number-of-args (cadr arities))))
		     (error "wrong arity for ~s" keyword))))))
      (loop with positions = (mapcar (lambda (x) (position x lambda-list))
				     to-process)
	    for keyword in to-process
	    for (pos next-pos) on (append positions
					  (list (list-structure lambda-list)))
	    do (check-arity keyword (- next-pos pos 1))))))
		     
(defun parse-ordinary-lambda-list (lambda-list processor processee)
  (let ((allowed '(&optional &rest &body &key &allow-other-keys &aux)))
    (check-lambda-list-proper lambda-list processor processee)
    (check-lambda-list-keywords lambda-list allowed processor processee)
    (let* ((real (remove-if-not (lambda (x) (member x allowed)) lambda-list))
	   (positions (append (mapcar (lambda (x) (position x lambda-list)) real)
			      (list (length lambda-list)))))
      (cons (subseq lambda-list 0 (car positions))
	    (loop for keyword in real
		  for (start end) on positions
		  collect (subseq lambda-list start end))))))
		   
