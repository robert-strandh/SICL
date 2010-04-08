;;; This code is in the public domain.
;;;
;;; The preliminary name for this project is SICL, which doesn't stand
;;; for anything in particular.  Pronounce it like "sickle".
;;;
;;; The purpose of this code is to provide a totally portable
;;; implementation of some high-level functionality of the Common Lisp
;;; language, so that implementors of Common Lisp systems can
;;; integrate it as it is into their systems, without having to
;;; implement and maintain a specific version of it. 
;;;
;;; Author: Robert Strandh (strandh@labri.fr)
;;; Date: 2008
;;;
;;; A portable implementation of the Common Lisp READ function.

;;; Ultimately, this form should be moved to a central place, such as
;;; packages.lisp.
(defpackage #:sicl-read
    (:use #:common-lisp)
  ;; For now, shadow these symbols.
  (:shadow #:read
	   #:*readtable*
	   #:readtable
	   #:readtablep
	   #:readtable-case
	   #:copy-readtable
	   #:char
	   #:get-macro-character
	   #:set-macro-character
	   ))

(in-package #:sicl-read)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro character functions

(defun single-quote-function (stream char)
  (declare (ignore char))
  (list 'quote (read stream t nil t)))

(defun double-quote-function (stream char)
  (declare (ignore char))
  (loop with result = (make-array 0
				  :element-type 'character
				  :adjustable t
				  :fill-pointer t)
	for new-char = (read-char stream nil nil)
	until (eql new-char #\")
	do (case new-char
	     ((nil) (error 'end-of-file :stream stream))
	     (#\\ (let ((next-char (read-char stream)))
		    (when (null next-char)
		      (error 'end-of-file :stream stream))
		    (vector-push-extend new-char result)))
	     (t (vector-push-extend new-char result)))
	finally (return (copy-seq result))))

(defun semicolon-function (stream char)
  (declare (ignore char))
  (loop for new-char = (read-char stream nil nil)
	until (or (null new-char) (eql new-char #\Newline))
	finally (return (values))))

(define-condition unmatched-right-parenthesis (reader-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Unmatched right parenthesis found"))))

(defun right-parenthesis-function (stream char)
  (declare (ignore stream char))
  (error 'unmatched-right-parenthesis))

;;; This condition is signaled when a token consisting
;;; only of (unescaped) dots was found.
(define-condition only-dots-in-token (reader-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "A token with only dots in it was found"))))

;;; This condition is signaled when a token consisting
;;; of a single (unescaped) dot was found.
(define-condition single-dot-token (only-dots-in-token)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "A token consisting of a single dot was found ~
                     in a context that does not permit such a token"))))

(define-condition no-object-preceding-dot (reader-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "A left parenthesis cannot be ~
                     immediately followed by a dot"))))

(defun left-parenthesis-function (stream char)
  (declare (ignore char))
  (let* ((sentinel (list nil))
	 (last-cons sentinel))
    (handler-case 
     (progn 
       (setf (cdr last-cons)
	     (handler-case (list (read stream t nil t))
	       (single-dot-token () (error 'no-object-preceding-dot))))
       (setf last-cons (cdr last-cons))
       (loop for expr = (handler-case (read stream t nil t)
			  (single-dot-token ()
			    (setf (cdr last-cons)
				  (read stream t nil t))
			    (return-from left-parenthesis-function
			      (cdr sentinel))))
	     do (setf (cdr last-cons) (list expr)
		      last-cons (cdr last-cons))))
     (unmatched-right-parenthesis ()
       (return-from left-parenthesis-function (cdr sentinel))))))

(defun backquote-function (stream char)
  (declare (ignore stream char))
  ;; define it
  )

(defun comma-function (stream char)
  (declare (ignore stream char))
  ;; define it
  )

(defun hash-function (stream char)
  (declare (ignore stream char))
  ;; define it
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dispatch macro character functions

(define-condition no-parameter-allowed ()
  ((%which-directive :initarg :which-directive :reader which-directive)
   (%parameter :initarg :parameter :reader parameter))
  (:report
   (lambda (condition stream)
     (format stream
	     "The ~a directive does not take a numeric parameter"
	     (which-directive condition)))))

(defun sharp-sign-single-quote-function (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (error 'no-parameter-allowed
	   :which-directive "#'"
	   :parameter parameter))
  (list 'function (read stream t nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Readtable

;;; Constituent-traits
(defparameter +invalid+                      #*00000000000001)
(defparameter +alphabetic+                   #*00000000000010)
(defparameter +alphadigit+                   #*00000000000100)
(defparameter +package-marker+               #*00000000001000)
(defparameter +plus-sign+                    #*00000000010000)
(defparameter +minus-sign+                   #*00000000100000)
(defparameter +dot+                          #*00000001000000)
(defparameter +decimal-point+                #*00000010000000)
(defparameter +ratio-marker+                 #*00000100000000)
(defparameter +float-exponent-marker+        #*00001000000000)
(defparameter +short-float-exponent-marker+  #*00010000000000)
(defparameter +single-float-exponent-marker+ #*00100000000000)
(defparameter +double-float-exponent-marker+ #*01000000000000)
(defparameter +long-float-exponent-marker+   #*10000000000000)

;;; Return true if and only of the object is a readtable
(defgeneric readtablep (object)
  (:method (object) nil))

;;; Return the readtable case, 
;;; one of :upcase, :downcase, :preserve, :invert
(defgeneric readtable-case (readtable))

;;; Set the readtable case, the case is
;;; one of :upcase, :downcase, :preserve, :invert
(defgeneric (setf readtable-case) (new-case readtable))

(defclass readtable ()
  ((%case :initform :upcase :accessor readtable-case)
   (%ascii-syntax-types :initform (make-array 128)
			:reader ascii-syntax-types)
   (%ascii-constituent-traits
    :initform (make-array 128 :initial-element #*0000000000000)
    :reader ascii-constituent-traits)
   (%other-syntax-types :initform (make-hash-table) :reader others)
   (%macro-functions :initform (make-hash-table) :reader macro-functions)))

(defun syntax-type (table char)
  (if (< (char-code char) 128)
      (aref (ascii-syntax-types table) (char-code char))
      (gethash char (others table) 'constituent)))

(defun (setf syntax-type) (new-type table char)
  (if (< (char-code char) 128)
      (setf (aref (ascii-syntax-types table) (char-code char)) new-type)
      (setf (gethash char (others table)) new-type)))

(defmethod readtablep ((object readtable))
  t)

;;; This function is not part of the visible Common Lisp API for
;;; readtables.
(defun make-readtable ()
  (make-instance 'readtable))

;;; Copy a readtable.  The default-value of the from-readtable
;;; optional argument is the current readtable.  The default value of
;;; to to-readtable argument is a fresh readtable.  If the value of the
;;; first argument is NIL, then restore the resulting readtable to
;;; standard syntax.
(defun copy-readtable (&optional (from-readtable *readtable*)
				 (to-readtable (make-readtable)))
  (setf (readtable-case to-readtable) (readtable-case from-readtable))
  (loop for i from 0 below 128
	do (setf (aref (ascii-syntax-types to-readtable) i)
		 (aref (ascii-syntax-types from-readtable) i))
	   (setf (aref (ascii-constituent-traits to-readtable) i)
		 (aref (ascii-constituent-traits from-readtable) i)))
  ;; copy the hash-tables
  )

(defun add-constituent-trait (table char trait)
  (setf (aref (ascii-constituent-traits table) (char-code char))
	(bit-ior (aref (ascii-constituent-traits table) (char-code char))
		 trait)))

(defun set-constituent-trait (table char trait)
  (setf (aref (ascii-constituent-traits table) (char-code char))
	trait))

(defun has-constituent-trait (table char trait)
  (let ((traits (if (< (char-code char) 128)
		    (aref (ascii-constituent-traits table)
			  (char-code char))
		    +alphabetic+)))
    (not (equal (bit-and trait traits) #*00000000000000))))

(defvar *standard-readtable*
  (let ((table (make-readtable)))
    ;; initialize all characters to be invalid constituent at first
    (loop for i from 0 below 128
	  do (setf (aref (ascii-syntax-types table) i)
		   'constituent)
	     (setf (aref (ascii-constituent-traits table) i)
		   +invalid+))
    ;; do the whitespace characters
    (loop for char in '(#\Backspace #\Newline #\Linefeed
			#\Page #\Return #\Space)
	  do (setf (aref (ascii-syntax-types table) (char-code char))
		   'whitespace))
    (loop for char in '(#\" #\' #\( #\) #\, #\; #\`)
	  do (setf (aref (ascii-syntax-types table) (char-code char))
		   'terminating-macro-char))
    (setf (aref (ascii-syntax-types table) (char-code #\#))
	  'non-terminating-macro-char)
    (setf (aref (ascii-syntax-types table) (char-code #\\))
	  'single-escape)
    (setf (aref (ascii-syntax-types table) (char-code #\|))
	  'multiple-escape)
    ;; diverse constituent characters
    (loop for char in '(#\! #\" #\# #\$ #\% #\& #\' #\( #\) #\*
			#\, #\; #\< #\= #\> #\? #\@ #\[ #\\ #\]
			#\^ #\_ #\` #\| #\~ #\{ #\} #\+ #\- #\. #\/)
	  do (set-constituent-trait table char +alphabetic+))
    (loop for code from (char-code #\0) to (char-code #\9)
	  do (set-constituent-trait table (code-char code) +alphadigit+))
    (loop for code from (char-code #\A) to (char-code #\Z)
	  do (set-constituent-trait table (code-char code) +alphadigit+))
    (loop for code from (char-code #\a) to (char-code #\z)
	  do (set-constituent-trait table (code-char code) +alphadigit+))
    (set-constituent-trait table #\: +package-marker+)
    (set-constituent-trait table #\+ +plus-sign+)
    (set-constituent-trait table #\- +minus-sign+)
    (set-constituent-trait table #\. +dot+)
    (set-constituent-trait table #\. +decimal-point+)
    (set-constituent-trait table #\/ +ratio-marker+)
    (set-constituent-trait table #\D +double-float-exponent-marker+)
    (set-constituent-trait table #\d +double-float-exponent-marker+)
    (set-constituent-trait table #\E +float-exponent-marker+)
    (set-constituent-trait table #\e +float-exponent-marker+)
    (set-constituent-trait table #\F +single-float-exponent-marker+)
    (set-constituent-trait table #\f +single-float-exponent-marker+)
    (set-constituent-trait table #\L +long-float-exponent-marker+)
    (set-constituent-trait table #\l +long-float-exponent-marker+)
    (set-constituent-trait table #\S +short-float-exponent-marker+)
    (set-constituent-trait table #\s +short-float-exponent-marker+)
    table))

(defvar *initial-readtable* (copy-readtable *standard-readtable*))

(defvar *readtable* *initial-readtable*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 

(defun get-macro-character (char &optional (readtable *readtable*))
  (let ((fun (gethash char (macro-functions readtable)))
	(syntax-type (syntax-type readtable char)))
    (values fun (eq syntax-type 'non-terminating-macro-char))))

(defun set-macro-character (char new-function
			    &optional
			    non-terminating-p
			    (readtable *readtable*))
  (setf (syntax-type readtable char)
	(if non-terminating-p
	    'non-terminating-macro-char
	    'terminating-macro-char))
  (setf (gethash char (macro-functions readtable)) new-function))
  
(mapc (lambda (pair) (set-macro-character (car pair)
					  (cdr pair)
					  nil
					  *standard-readtable*))
      `((#\' . ,#'single-quote-function)
	(#\" . ,#'double-quote-function)
	(#\; . ,#'semicolon-function)
	(#\) . ,#'right-parenthesis-function)
	(#\( . ,#'left-parenthesis-function)
	(#\` . ,#'backquote-function)
	(#\, . ,#'comma-function)))

(set-macro-character #\# #'hash-function t *standard-readtable*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader algorithm

(define-condition invalid-character (reader-error)
  ((%char :initarg :char :reader char))
  (:report
   (lambda (condition stream)
     (format stream
	     "Invalid character ~s on stream ~s."
	     (char condition)
	     (stream-error-stream condition)))))
(defun read (&optional
	     input-stream
	     (eof-error-p t)
	     (eof-value nil)
	     (recursive-p nil))
  (setf input-stream
	(cond ((eq input-stream t) *terminal-io*)
	      ((null input-stream) *standard-input*)
	      (t input-stream)))
  (let ((table *readtable*)
	char syntax-type token)
  (tagbody
     1
     (setf char (read-char input-stream eof-error-p eof-value recursive-p))
     (setf syntax-type (syntax-type table char))
     (cond ((and (eq syntax-type 'constituent)
		 (has-constituent-trait table char +invalid+))
	    ;; do this better
	    (error 'invalid-character :stream input-stream :char char))
	   ((eq syntax-type 'whitespace)
	    (go 1))
	   ((or (eq syntax-type 'terminating-macro-char)
		(eq syntax-type 'non-terminating-macro-char))
	    (let* ((fun (gethash char (macro-functions table)))
		   (values (multiple-value-list
			      (funcall fun input-stream char))))
	      (if (null values)
		  (go 1)
		  (return-from read (first values)))))
	   ((eq syntax-type 'single-escape)
	    ;; do this better by signaling a more specific condition.
	    (setf char (read-char input-stream t nil t))
	    (setf token (make-array 1
				    :initial-element char
				    :element-type 'character
				    :adjustable t
				    :fill-pointer 1))
	    (go 8))
	   ((eq syntax-type 'multiple-escape)
	    (setf token (make-array 0
				    :element-type 'character
				    :adjustable t
				    :fill-pointer 0))
	    (go 9))
	   ((eq syntax-type 'constituent)
	    ;; handle case conversion, etc
	    (setf token (make-array 1
				    :initial-element char
				    :element-type 'character
				    :adjustable t
				    :fill-pointer 1))
	    (go 8)))
     8
     (setf char (read-char input-stream nil nil t))
     (if (null char)
	 (go 10)
	 (progn (setf syntax-type (syntax-type table char))
		(cond ((or (eq syntax-type 'constituent)
			   (eq syntax-type 'non-terminating-macro-char))
		       (vector-push-extend char token)
		       (go 8))
		      ((eq syntax-type 'single-escape)
		       ;; do this better by signaling a	
		       ;; more specific condition.
		       (setf char (read-char input-stream t nil t))
		       (vector-push-extend char token)
		       (go 8))
		      ((eq syntax-type 'multiple-escape)
		       (go 9))
		      ((and (eq syntax-type 'constituent)
			    (has-constituent-trait table char +invalid+))
		       ;; do this better
		       (error 'invalid-character
			      :stream input-stream :char char))
		      ((eq syntax-type 'terminating-macro-char)
		       (unread-char char input-stream)
		       (go 10))
		      ((eq syntax-type 'whitespace)
		       ;; check for preserving whitespace
		       (unread-char char input-stream)
		       (go 10)))))
     9
     ;; do this better by signaling a
     ;; more specific condition.
     (setf char (read-char input-stream t nil t))
     (setf syntax-type (syntax-type table char))
     (cond ((or (eq syntax-type 'constituent)
		(eq syntax-type 'whitespace))
	    (vector-push-extend char token)
	    (go 9))
	   ((eq syntax-type 'single-escape)
	    ;; do this better by signaling a	
	    ;; more specific condition.
	    (setf char (read-char input-stream t nil t))
	    (vector-push-extend char token)
	    (go 9))
	   ((eq syntax-type 'multiple-escape)
	    (go 8))
	   ((and (eq syntax-type 'constituent)
		 (has-constituent-trait table char +invalid+))
	    ;; do this better
	    (error 'invalid-character
		   :stream input-stream :char char)))
     10
     ;; build the token here
     )
    token))
