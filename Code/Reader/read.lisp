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
	   ))

(in-package #:sicl-read)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Character syntax-types

(defclass syntax-type () ())

(defclass whitespace (syntax-type) ())

(defclass constituent (syntax-type)
  ((%traits :initarg :traits :reader traits)))

(defclass macro-char (syntax-type)
  ((%dispatch-function :initarg dispatch-function :reader dispatch-function)))

(defclass terminating-macro-char (macro-char) ())

(defclass non-terminating-macro-char (macro-char) ())

(defclass single-escape (syntax-type) ())

(defclass multiple-escape (syntax-type) ())

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

(defun unmatched-right-parenthesis (reader-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Unmatched right parenthesis found"))))

(defun right-parenthesis-function (stream char)
  (delcare (ignore char))
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
     (decalre (ignore condition))
     (format stream "A token consisting of a single dot was found ~
                     in a context that does not permit such a token"))))

(define-condition no-object-preceding-dot (reader-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "A left parenthesis cannot be ~
                     immediately followed by a dot"))))

(defun left-parenthesis-function (stream char)
  (declare (ignore char))
  (let* ((sentinel (list nil))
	 (last-cons sentinel))
    (handler-case 
     (progn 
       (setf (cdr last-cons)
	     (handler-case (read stream t nil t)
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
  (unless (null parameter)
    (error 'no-parameter-allowed
	   :which-directive "#'"
	   :parameter parameter))
  (list 'function (read stream t nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Readtable

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
   (%ascii :initform (make-array 128) :reader ascii)
   (%others :initform (make-hash-table) :reader others)
   (%default-syntax-class :initform (make-instance 'constituent
				      :traits '(alphabetic))
			  :reader default-syntax-class)))

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
  ;; FIXME: Implement this function
  (declare (ignore from-readtable to-readtable))
  nil)

(defvar *initial-readtable*
  (let ((table (make-readtable)))
    ;; initial all characters to be invalid at first
    (loop for i from 0 below 128
	  do (setf (aref (ascii table) i)
		   (make-instance 'constituent
		     :traits '(invalid))))
    ;; do the whitespace characters
    (loop for char in '(#\Backspace #\Newline #\Linefeed
			#\Page #\Return #\Space)
	  do (setf (aref (ascii table) (char-code char))
		   (make-instance 'whitespace)))
    ;; diverse constituent characters
    (loop for char in '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> 
			#\? #\@ #\[ #\] #\^ #\_ #\{ #\} #\~ #\Rubout)
	  do (setf (aref (ascii table) (char-code char))
		   (make-instance 'constituent
		     :traits '(alphabetic))))
    (setf (aref (ascii table) (char-code #\+))
	  (make-instance 'constituent
	    :traits '(alphabetic plus-sign)))
    (setf (aref (ascii table) (char-code #\-))
	  (make-instance 'constituent
	    :traits '(alphabetic minus-sign)))
    (setf (aref (ascii table) (char-code #\.))
	  (make-instance 'constituent
	    :traits '(alphabetic dot decimal-point)))
    (setf (aref (ascii table) (char-code #\:))
	  (make-instance 'constituent
	    :traits '(alphabetic package-marker)))
    (setf (aref (ascii table) (char-code #\/))
	  (make-instance 'constituent
	    :traits '(alphabetic ratio-marker)))
    ;; do the letters
    (loop for i from (char-code #\A) to (char-code #\Z)
	  do (setf (aref (ascii table) i)
		   (make-instance 'constituent
		     :traits '(alphadigit))))
    ;; do the letters
    (loop for i from (char-code #\a) to (char-code #\z)
	  do (setf (aref (ascii table) i)
		   (make-instance 'constituent
		     :traits '(alphadigit))))
    ;; do the digits
    (loop for i from (char-code #\0) to (char-code #\9)
	  do (setf (aref (ascii table) i)
		   (make-instance 'constituent
		     :traits '(alphadigit))))

    table))

(defvar *readtable* *initial-readtable*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader state

(defclass reader-state ()
  ((%preserve-whitespace-p :initarg :preserve-whitespace-p
			   :initform nil
			   :reader preserve-whitespace-p)))

(defclass initial (reader-state) ())

(defclass token-accumulation (reader-state)
  ((%chars-so-far :initarg :chars-so-far :reader chars-so-far)
   ;; a bit vector with the same length as chars-so-far, and
   ;; which indicates whether the i-eth character was escaped. 
   (%escaped-chars :initarg :escaped-chars :reader escaped-chars)))

(defclass odd-number-of-escapes (token-accumulation) ())

(defclass even-number-of-escapes (token-accumulation) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader algorithm

(defgeneric read-internal (stream char syntax-class reader-state))

(defmethod read-internal (stream
			  char
			  (syntax-class whitespace)
			  (reader-state initial))
  ;; the HyperSpec says we discard the character and
  ;; re-enter the initial state
  reader-state)

(defmethod read-internal (stream
			  char
			  (syntax-class macro-char)
			  (reader-state initial))
  (multiple-value-call
      (lambda (&optional (value nil value-p))
	(when value-p
	  (values value t)))
    (funcall (dispatch-function syntax-class)
	     stream char)))

(defmethod read-internal (stream
			  char
			  (syntax-class single-escape)
			  (reader-state initial))
  (let ((new-char (read-char stream nil nil)))
    (when (null new-char)
      (error 'end-of-file :stream stream))
    (change-class reader-state
		  'even-number-of-escapes
		  :chars-so-far (make-array 1
					    :element-type 'character
					    :initial-element new-char
					    :adjustable t
					    :fill-pointer t)
		  :escaped-chars (make-array 1
					     :element-type 'bit
					     :initial-element 1
					     :adjustable t
					     :fill-pointer t))))

(defmethod read-internal (stream
			  char
			  (syntax-class multiple-escape)
			  (reader-state initial))
  (change-class reader-state
		'odd-number-of-escapes
		:chars-so-far (make-array 0
					  :element-type 'character
					  :adjustable t
					  :fill-pointer t)
		:escaped-chars (make-array 0
					   :element-type 'bit
					   :adjustable t
					   :fill-pointer t)))

(defmethod read-internal (stream
			  char
			  (syntax-class constituent)
			  (reader-state initial))
  (when (member :invalid (traits syntax-class))
    (error 'reader-error :stream stream))
  (change-class reader-state
		'even-number-of-escapes
		:chars-so-far (make-array 1
					  :element-type 'character
					  :initial-element char
					  :adjustable t
					  :fill-pointer t)
		:escaped-chars (make-array 1
					   :element-type 'bit
					   :initial-element 0
					   :adjustable t
					   :fill-pointer t)))

(defmethod read-internal (stream
			  char
			  (syntax-class constituent)
			  (reader-state even-number-of-escapes))
  (when (member :invalid (traits syntax-class))
    (error 'reader-error :stream stream))
  (vector-push-extend final-char (chars-so-far reader-state))
  (vector-push-extend 0 (escaped-chars reader-state)))

(defmethod read-internal (stream
			  char
			  (syntax-class non-terminating-macro-char)
			  (reader-state even-number-of-escapes))
  (vector-push-extend final-char (chars-so-far reader-state))
  (vector-push-extend 0 (escaped-chars reader-state)))

(defmethod read-internal (stream
			  char
			  (syntax-class single-escape)
			  (reader-state token-accumulation))
  (let ((new-char (read-char stream nil nil)))
    (when (null new-char)
      (error 'end-of-file :stream stream))
    (vector-push-extend new-char (chars-so-far reader-state))
    (vector-push-extend 1 (escaped-chars reader-state))))

(defmethod read-internal (stream
			  char
			  (syntax-class multiple-escape)
			  (reader-state even-number-of-escapes))
  (change-class reader-state 'odd-number-of-escapes))


(defun make-token (string)
  ;; FIXME check for number or potential number
  string)

(defmethod read-internal (stream
			  char
			  (syntax-class terminating-macro-char)
			  (reader-state even-number-of-escapes))
  (unread-char char stream)
  (values (make-token (chars-so-far reader-state)) t))

(defmethod read-internal (stream
			  char
			  (syntax-class whitespace)
			  (reader-state even-number-of-escapes))
  (when (preserve-whitespace-p reader-state)
    (unread-char char stream))
  (values (make-token (chars-so-far reader-state)) t))

(defmethod read-internal (stream
			  char
			  (syntax-class constituent)
			  (reader-state odd-number-of-escapes))
  (when (member :invalid (traits syntax-class))
    (error 'reader-error :stream stream))
  (vector-push-extend char (chars-so-far reader-state))
  (vector-push-extend 1 (escaped-chars reader-state)))
  
(defmethod read-internal (stream
			  char
			  (syntax-class macro-char)
			  (reader-state odd-number-of-escapes))
  (vector-push-extend char (chars-so-far reader-state))
  (vector-push-extend 1 (escaped-chars reader-state)))
  

(defmethod read-internal (stream
			  char
			  (syntax-class whitespace)
			  (reader-state odd-number-of-escapes))
  (vector-push-extend char (chars-so-far reader-state))
  (vector-push-extend 1 (escaped-chars reader-state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main entry point

(defun syntax-class (char)
  (if (<= (char-code char) 127)
      (aref (ascii *readtable*) (char-code char))
      (let ((val (gethash char (others *readtable*))))
	(or val (default-syntax-class *readtable*)))))

(defun read (&optional input-stream (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (setf input-stream
	(cond ((null input-stream)
	       *standard-input*)
	      ((eq input-stream t)
	       *terminal-io*)
	      (t input-stream)))
  (let ((state (make-instance 'initial)))
    (loop with endp = nil
	  with result = nil
	  do (let ((char (read-char input-stream nil nil)))
	       (if (null char)
		   (if (typep state 'even-number-of-escapes)
		       (return (make-token (chars-so-far state)))
		       (if eof-error-p
			   (error 'end-of-file :stream input-stream)
			   (return eof-value)))
		   (multiple-value-bind (value value-p)
		       (read-internal input-stream
				      char
				      (syntax-class char)
				      state)
		     (setf endp value-p
			   result value))))
	  until endp
	  finally (return result))))

