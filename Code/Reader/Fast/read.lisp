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
;;; Author: Robert Strandh (robert.strandh@gmail.com)
;;; Date: 2008
;;;
;;; A portable implementation of nearly all of the Common Lisp READ
;;; function.  Interestingly, it is not possible to implement #= and
;;; ## portably, because of the following property: When there is a
;;; label on an object that has not yet been built, and a reference to
;;; that label inside the object itself, such as #1=(#1#), when the
;;; #1# is seen, it cannot be resolved, so some kind of temporary
;;; value must be put there instead.  Later, this value must be
;;; replaced by a reference to the object ultimately built.  I can see
;;; two ways of doing that: One way would be for READ to can scan the
;;; object recursively to search for the value to be replaced.  But
;;; this cannot be done portably, because functions from the 
;;; Meta-Object Protocol are needed to scan instances of for example
;;; standard-object.  Onother way would be to allow for a particular
;;; value to stand for some indirection to a different value and
;;; trap any attempt to return this value in a way similar to 
;;; how unbound variables are handled.  Clearly, this second method
;;; requires deep knowledge about some of the most basic mechanisms
;;; of the implementation, so that method is not portable either. 
;;; For now, we just leave it up to each system to implement
;;; #= and ##. 

(cl:in-package #:sicl-read)

(defparameter *readtable* nil)

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
	do (cond 
	     ((null new-char)
	      (error 'end-of-file :stream stream))
	     ((eq (syntax-type *readtable* new-char) 'single-escape)
	      (let ((next-char (read-char stream nil nil)))
		(when (null next-char)
		  (error 'end-of-file :stream stream))
		(vector-push-extend next-char result)))
	     (t (vector-push-extend new-char result)))
	finally (return (copy-seq result))))

(defun semicolon-function (stream char)
  (declare (ignore char))
  (loop for new-char = (read-char stream nil nil t)
	until (or (null new-char) (eql new-char #\Newline))
	finally (return (values))))

(defun right-parenthesis-function (stream char)
  (declare (ignore char))
  (error 'unmatched-right-parenthesis :stream stream))

(defun left-parenthesis-function (stream char)
  (declare (ignore char))
  (let* ((sentinel (list nil))
	 (last-cons sentinel))
    ;; We continue reading subexpressions
    ;; until we get informed by the condition
    ;; unmatched-right-parenthesis that a right
    ;; parenthesis was found. 
    (handler-case 
     (progn 
       ;; Read a first subexpression.  If we find a single dot
       ;; token (which is indicated by the condition single-dot-token
       ;; being signaled from the recursive read), then we in 
       ;; turn signal the no-object-preceding-dot condition.
       (setf (cdr last-cons)
	     (handler-case (list (read stream t nil t))
	       (single-dot-token ()
		 (error 'no-object-preceding-dot :stream stream))))
       ;; Come here when we successfully read the first subexpression
       ;; of the list. 
       (setf last-cons (cdr last-cons))
       ;; Continue reading more subexpressions, appending
       ;; them to the end of the list we are accumulating.
       ;; If when reading such an expression, we get informed,
       ;; by the single-dot-token condition being signaled,
       ;; that a single dot has been found.  Enter into a final 
       ;; processing stage where two expression are read,
       ;; the first one which must a normal expression
       ;; following the dot, and the second one must
       ;; result in the signal unmatched-right-parenthesis
       ;; being signaled. 
       (loop for expr = (handler-case (read stream t nil t)
			  (single-dot-token ()
			    (setf (cdr last-cons)
				  (read stream t nil t))
			    (handler-case
				(let ((exp (read stream t nil t)))
				  ;; An expression was successfully
				  ;; read, which shouldn't happen.
				  (error 'multiple-objects-following-dot
					 :stream stream
					 :offending-expression exp))
			      (unmatched-right-parenthesis ()
				(return-from left-parenthesis-function
				  (cdr sentinel))))))
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

(defun sharpsign-function (stream char)
  (declare (ignore stream char))
  ;; define it
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dispatch macro character functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sharpsign single quote

(defun sharpsign-single-quote-function (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (error 'no-parameter-allowed
	   :stream stream
	   :which-directive "#'"
	   :parameter parameter))
  (list 'function (read stream t nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sharpsign backslash

(defparameter *character-names*
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash "NEWLINE" table) #\Newline)
    (setf (gethash "SPACE" table) #\Space)
    (setf (gethash "RUBOUT" table) #\Rubout)
    (setf (gethash "PAGE" table) #\Page)
    (setf (gethash "TAB" table) #\Tab)
    (setf (gethash "BACKSPACE" table) #\Backspace)
    (setf (gethash "RETURN" table) #\Return)
    (setf (gethash "LINEFEED" table) #\Linefeed)
    table))

(defun sharpsign-backslash-function (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (error 'no-parameter-allowed
	   :stream stream
	   :which-directive "#\\"
	   :parameter parameter))
  (let ((char (read-char stream nil nil t)))
    (when (null char)
      (error 'end-of-file :stream stream))
    (let ((next-char (read-char stream nil nil t)))
      (cond ((null next-char)
	     char)
	    ((not (eq (syntax-type *readtable* next-char) 'constituent))
	     (unread-char next-char stream)
	     char)
	    (t
	     (let ((name (make-array 10 :element-type 'character :fill-pointer 2)))
	       (setf (aref name 0) char
		     (aref name 1) next-char)
	       (loop for char = (read-char stream nil nil t)
		     while (and (not (null char))
				(eq (syntax-type *readtable* char) 'constituent))
		     do (vector-push-extend char name)
		     finally (progn (unless (null char)
				      (unread-char char stream))
				    (let ((char (gethash (string-upcase name)
							 *character-names*)))
				      (when (null char)
					(error 'unknown-character-name
					       :stream stream
					       :name name))
				      (return char))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sharpsign left parenthesis

(defun sharpsign-left-parenthesis (stream char parameter)
  (if (null parameter)
      ;; If no parameter is specified, we read a list
      ;; of elements that we use as initialization for 
      ;; the vector to create
      (progn (unread-char char stream)
	     (let ((contents (read stream t nil t)))
	       (make-array (length contents) :initial-contents contents)))
      (let ((vector (make-array parameter))
	    (i -1))
	(loop for expr = (handler-case
			     (setf (aref vector (incf i))
				   (read stream t nil t))
			   (unmatched-right-parenthesis ()
			     (loop-finish)))
	      while (< i (1- parameter)))
	(if (< -1 i (1- parameter))
	    ;; We must copy the last element read
	    (loop for j from (1+ i) below parameter
		  do (setf (aref vector j) (aref vector i)))
	    ;; Come here if we haven't seen the right
	    ;; parenthesis yet.  Skip the remaining elements
	    ;; which is allowed by the spec.
	    (loop for expr = (handler-case
				 (setf (aref vector (incf i))
				       (read stream t nil t))
			       (unmatched-right-parenthesis ()
				 (loop-finish)))))
	vector)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Readtable

;;; Constituent-traits
(defparameter +invalid+                       0)
(defparameter +alphabetic+                    1)
(defparameter +alphadigit+                    2)
(defparameter +package-marker+                3)
(defparameter +plus-sign+                     4)
(defparameter +minus-sign+                    5)
(defparameter +dot+                           6)
(defparameter +decimal-point+                 7)
(defparameter +ratio-marker+                  8)
(defparameter +float-exponent-marker+         9)
(defparameter +short-float-exponent-marker+  10)
(defparameter +single-float-exponent-marker+ 11)
(defparameter +double-float-exponent-marker+ 12)
(defparameter +long-float-exponent-marker+   13)

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
  ((%case :initarg :case :accessor readtable-case)
   ;; A vector of symbols, each symbol being the name
   ;; of a syntax type: constituent, whitespace, 
   ;; terminating-macro-char, non-terminating-macro-char,
   ;; single-escape, or multiple-escape.
   (%ascii-syntax-types :initarg :ascii-syntax-types
			:reader ascii-syntax-types)
   ;; A hash table of syntax types for characters other
   ;; than ASCII.  By default, we assume that the
   ;; syntax type is constituent, so that this hash table
   ;; only contains entries for characters that aren't
   ;; constituent. 
   (%other-syntax-types :initform (make-hash-table) :reader others)
   ;; A vector of bit vectors.  Each bit vector has 14 element,
   ;; each element corresponding to a constituent trait.  If
   ;; the element is 1, then the character has the corresponding
   ;; constituent trait.  Otherwise it does not.
   (%ascii-constituent-traits :initarg :ascii-constituent-traits
			      :reader ascii-constituent-traits)
   ;; A bit vector of length 128 indicating whether an ASCII character
   ;; is sure to indicate a symbol, should that character appear first
   ;; in a token when the input radix is 10.  This provides a quick
   ;; test for an important special case.  We exclude the package marker
   ;; which we have to test for explicitly. 
   (%decimal-letters :initarg :decimal-letters :reader decimal-letters)
   ;; A bit vector of length 128 indicating whether an ASCII character
   ;; is a decimal digit.
   (%decimal-digits :initarg :decimal-digits :reader decimal-digits)
   ;; A bit vector of length 128 indicating whether an ASCII character
   ;; has whitespace syntax.
   (%whitespace :initarg :whitespace :reader whitespace)
   ;; A hash table associating characters with macro functions.
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

(defun has-constituent-trait-p (table char trait)
  (let ((code (char-code char)))
    (if (< code 128)
	(= (aref (aref (ascii-constituent-traits table) code) trait) 1)
	(= trait +alphabetic+))))

(defparameter *standard-readtable*
  (let ((ascii-syntax-types
	 (let ((types (make-array 128 :initial-element 'constituent)))
	   
	   (loop for char in '(#\Backspace #\Newline #\Linefeed
			       #\Page #\Return #\Space)
		 do (setf (aref types (char-code char))
			  'whitespace))
	   (loop for char in '(#\" #\' #\( #\) #\, #\; #\`)
		 do (setf (aref types (char-code char))
			  'terminating-macro-char))
	   (setf (aref types (char-code #\#))
		 'non-terminating-macro-char)
	   (setf (aref types (char-code #\\))
		 'single-escape)
	   (setf (aref types (char-code #\|))
		 'multiple-escape)
	   types))
	(ascii-constituent-traits
	 (let ((traits (make-array 128)))
	   (flet ((add-char-trait (char trait)
		    (setf (aref (aref traits (char-code char)) +invalid+) 0)
		    (setf (aref (aref traits (char-code char)) trait) 1))
		  (add-code-trait (code trait)
		    (setf (aref (aref traits code) +invalid+) 0)
		    (setf (aref (aref traits code) trait) 1)))
	     (loop for i from 0 below 128
		   do (let ((bv (make-array 14 :element-type 'bit
					    :initial-element 0)))
			(setf (aref bv +invalid+) 1)
			(setf (aref traits i) bv)))
	     (loop for char in '(#\! #\" #\# #\$ #\% #\& #\' #\( #\) #\*
				 #\, #\; #\< #\= #\> #\? #\@ #\[ #\\ #\]
				 #\^ #\_ #\` #\| #\~ #\{ #\} #\+ #\- #\. #\/)
		   do (add-char-trait char +alphabetic+))
	     (loop for code from (char-code #\0) to (char-code #\9)
		   do (add-code-trait code +alphadigit+))
	     (loop for code from (char-code #\A) to (char-code #\Z)
		   do (add-code-trait code +alphadigit+))
	     (loop for code from (char-code #\a) to (char-code #\z)
		   do (add-code-trait code +alphadigit+))
	     (add-char-trait #\: +package-marker+)
	     (add-char-trait #\+ +plus-sign+)
	     (add-char-trait #\- +minus-sign+)
	     (add-char-trait #\. +dot+)
	     (add-char-trait #\. +decimal-point+)
	     (add-char-trait #\/ +ratio-marker+)
	     (add-char-trait #\D +double-float-exponent-marker+)
	     (add-char-trait #\d +double-float-exponent-marker+)
	     (add-char-trait #\E +float-exponent-marker+)
	     (add-char-trait #\e +float-exponent-marker+)
	     (add-char-trait #\F +single-float-exponent-marker+)
	     (add-char-trait #\f +single-float-exponent-marker+)
	     (add-char-trait #\L +long-float-exponent-marker+)
	     (add-char-trait #\l +long-float-exponent-marker+)
	     (add-char-trait #\S +short-float-exponent-marker+)
	     (add-char-trait #\s +short-float-exponent-marker+))
	   traits))
	(decimal-letters
	 (let ((letters (make-array 128 :element-type 'bit :initial-element 0)))
	   (loop for code from (char-code #\A) to (char-code #\Z)
		 do (setf (aref letters code) 1))
	   (loop for code from (char-code #\a) to (char-code #\z)
		 do (setf (aref letters code) 1))
	   (loop for char in '(#\! #\$ #\% #\& #\* #\/ #\< #\= #\>
			       #\? #\@ #\[ #\] #\^ #\_ #\{ #\} #\~)
		 do (setf (aref letters (char-code char)) 1))
	   letters))
	(decimal-digits
	 (let ((digits (make-array 128 :element-type 'bit :initial-element 0)))
	   (loop for code from (char-code #\0) to (char-code #\9)
		 do (setf (aref digits code) 1))
	   digits))
	(whitespace
	 (let ((whitespace (make-array 128 :element-type 'bit :initial-element 0)))
	   (loop for char in '(#\Backspace #\Tab #\Newline
			       #\Linefeed #\Page #\Return #\Space)
		 do (setf (aref whitespace (char-code char)) 1))
	   whitespace)))
    (make-instance 'readtable
		   :case :upcase
		   :ascii-syntax-types ascii-syntax-types
		   :ascii-constituent-traits ascii-constituent-traits
		   :decimal-letters decimal-letters
		   :decimal-digits decimal-digits
		   :whitespace whitespace)))

;;; Copy a readtable.  The default-value of the from-readtable
;;; optional argument is the current readtable.  The default value of
;;; to to-readtable argument is a fresh readtable.  If the value of the
;;; first argument is NIL, then restore the resulting readtable to
;;; standard syntax.
(defun copy-readtable (&optional (from-readtable *readtable*) to-readtable)
  (when (null from-readtable)
    (setf from-readtable *standard-readtable*))
  (flet ((copy-hashtables (from to)
	   (clrhash (others to))
	   (loop for key being each hash-key of (others from)
		 using (hash-value value)
		 do (setf (gethash key (others to)) value))
	   (clrhash (macro-functions to))
	   (loop for key being each hash-key of (macro-functions from)
		 using (hash-value value)
		 do (setf (gethash key (macro-functions to)) value))))
    (if (null to-readtable)
	(let* ((ascii-syntax-types
		(copy-seq (ascii-syntax-types from-readtable)))
	       (ascii-constituent-traits
		(let ((copy (copy-seq (ascii-constituent-traits from-readtable))))
		  (loop for i from 0 below 128
			do (setf (aref copy i) (copy-seq (aref copy i))))
		  copy))
	       (decimal-letters
		(copy-seq (decimal-letters from-readtable)))
	       (decimal-digits
		(copy-seq (decimal-digits from-readtable)))
	       (whitespace
		(copy-seq (whitespace from-readtable)))
	       (new-readtable (make-instance 'readtable
				:case (readtable-case from-readtable)
				:ascii-syntax-types ascii-syntax-types
				:ascii-constituent-traits ascii-constituent-traits
				:decimal-letters decimal-letters
				:decimal-digits decimal-digits
				:whitespace whitespace)))
	  (copy-hashtables from-readtable new-readtable)
	  new-readtable)
	(progn
	  (loop for i from 0 to 128
		do (setf (aref (ascii-syntax-types to-readtable) i)
			 (aref (ascii-syntax-types from-readtable) i))
		   (setf (aref (ascii-constituent-traits to-readtable) i)
			 (copy-seq (aref (ascii-constituent-traits from-readtable) i)))
		   (setf (sbit (decimal-letters to-readtable) i)
			 (sbit (decimal-letters from-readtable) i))
		   (setf (sbit (decimal-digits to-readtable) i)
			 (sbit (decimal-digits from-readtable) i))
		   (setf (sbit (whitespace to-readtable) i)
			 (sbit (whitespace from-readtable) i)))
	  (setf (readtable-case to-readtable) (readtable-case from-readtable))
	  (copy-hashtables from-readtable to-readtable)
	  to-readtable))))

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
  ;; Make sure the character is no longer considered to be
  ;; a symbol starter. 
  (setf (aref (decimal-letters readtable) (char-code char)) 0)
  (setf (gethash char (macro-functions readtable)) new-function))
  
(mapc (lambda (pair) (set-macro-character (car pair)
					  (cdr pair)
					  nil
					  *standard-readtable*))
      `((#\' . single-quote-function)
	(#\" . double-quote-function)
	(#\; . semicolon-function)
	(#\) . right-parenthesis-function)
	(#\( . left-parenthesis-function)
	(#\` . backquote-function)
	(#\, . comma-function)))

(set-macro-character #\# #'sharpsign-function t *standard-readtable*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; In order to preserve the source-code positions of the expressions
;;; read, we define a class that contains the expression itself, a
;;; start and an end position, and a list of subexpressions.

(defclass expression ()
  ((%start :initarg :start :reader start)
   (%end :initarg :end :reader end)
   (%expression :initarg :expression :reader expression)
   (%sub-expressions :initarg :sub-expressions :reader sub-expressions)))

(defmethod print-object ((exp expression) stream)
  (format stream "[(~a ~a) ~s ~s]"
          (start exp)
          (end exp)
          (expression exp)
          (sub-expressions exp)))

(defparameter *expression-stack* '())

(defun push-expression-stack ()
  (unless (null *expression-stack*)
    (push '() *expression-stack*)))

(defun pop-expression-stack ()
  (unless (null *expression-stack*)
    (pop *expression-stack*)))

(defun combine-expression-stack (expression start end)
  (unless (null *expression-stack*)
    (push (make-instance 'expression
                         :start start
                         :end end
                         :expression expression
                         :sub-expressions
                         (nreverse (car *expression-stack*)))
          (cadr *expression-stack*))))


;;; This variable is set to true by read-with-position.
;;; The read function checks this value to see whether
;;; to call the expression-stack functions. 

(defparameter *read-with-position* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader algorithm

;;; The use of vector-push-extend makes it very slow to accumulate
;;; tokens.  Instead, use a fixed buffer of significant size
;;; consisting of a simple array, and then if the fixed buffer
;;; overflows, move the contents somewhere else, and use a slower
;;; method.  This method has the additional advantage of allocating
;;; much less memory.

(defparameter *buffer* (make-array 100 :element-type 'character))

;;; Upper-case versions of every ascii character.  For fast test.
(defparameter *ascii-upcase*
  (let ((vector (make-array 128 :element-type 'character)))
    (loop for i from 0 below 128
	  do (setf (aref vector i)
		   (char-upcase (code-char i))))
    vector))

;;; Lower-case versions of every ascii character.  For fast test.
(defparameter *ascii-downcase*
  (let ((vector (make-array 128 :element-type 'character)))
    (loop for i from 0 below 128
	  do (setf (aref vector i)
		   (char-downcase (code-char i))))
    vector))

;;; The identity mapping, so as to avoid special-casing this
;;; (supposedly uncommon) case.
(defparameter *ascii-preserve*
  (let ((vector (make-array 128 :element-type 'character)))
    (loop for i from 0 below 128
	  do (setf (aref vector i)
		   (code-char i)))
    vector))

;;; A special version of the read function that assumes that the input
;;; radix is 10 and that the readtable case of the current readtable
;;; is one of :upcase, :downcase, and :preserve.
(defun read-upcase-downcase-preserve-decimal
    (input-stream eof-error-p eof-value recursive-p case-table case-function)
  (let* ((table *readtable*)
	 (buffer *buffer*)
	 (buffer-size (length buffer))
	 (read-with-position *read-with-position*)
	 (index 0)
	 (whitespace (whitespace table))
	 (decimal-letters (decimal-letters table))
	 (decimal-digits (decimal-digits table))
	 ;; for symbol accumulation
	 (first-package-marker-position nil)
	 (second-package-marker-position nil)
	 ;; for number accumulation
	 (sign 1)
	 (numerator 0)
	 (denominator 0)
	 (scale 0)
	 (exponent 0)
	 (float-prototype (case *read-default-float-format*
			    (short-float 1s0)
			    (single-float 1f0)
			    (double-float 1d0)
			    (long-float 1l0)))
	 char
	 code
	 start)
    (declare (type simple-string buffer)
	     (type (simple-bit-vector 128)
		   whitespace decimal-letters decimal-digits)
	     (type (simple-string 128) case-table))
    (flet ((increase-buffer ()
	     (let ((new-buffer (make-array (* 2 buffer-size)
					   :element-type 'character)))
	       (setf (subseq new-buffer 0 buffer-size) buffer)
	       (setf buffer new-buffer)
	       (setf buffer-size (length new-buffer)))))
      (macrolet ((push-char (source)
		   `(progn (when (= index buffer-size)
			     (increase-buffer))
			   (setf (schar buffer index) ,source)
			   (incf index))))
	(tagbody
	   ;; The initial state.
	   (setf char (read-char input-stream eof-error-p eof-value recursive-p))
	   (setf code (char-code char))
	   ;; Skip whitespace.
	   (loop while (and (< code 128)
			    (= (sbit whitespace code) 1))
		 do (setf char (read-char input-stream eof-error-p eof-value recursive-p))
		    (setf code (char-code char)))
	   (when read-with-position
	     ;; It is not safe to add to or subtract from the file position,
	     ;; so the only method that is portable is to undread the
	     ;; character, determine the file position, and then read the
	     ;; character again.
	     (unread-char char input-stream)
	     (setf start (file-position input-stream))
	     (read-char input-stream eof-error-p eof-value recursive-p))
	   ;; Come here when we know that we do not have an ASCII whitespace.
	   ;; The variable char contains the first character wich is not
	   ;; an ASCII whitespace, and the variable `code' contains the
	   ;; char-code of that character.
	   (if (< code 128)
	       ;; Then not only do we not have an ASCII whitespace, but
	       ;; we have no whitespace at all.
	       (progn 
		 (cond (;; We make a quick test to see if it must be a symbol.
			(= (sbit decimal-letters code) 1)
			;; There is no point in testing whether the buffer
			;; is full at this point, because we have only processed
			;; a single character.
			(setf (schar buffer index) (schar case-table code))
			(incf index)
			(go symbol-even-escape-no-package-marker))
		       (;; Another common case is that we have a digit
			(= (sbit decimal-digits code) 1)
			;; There is no point in testing whether the buffer
			;; is full at this point, because we have only processed
			;; a single character.
			(setf (schar buffer index) char)
			(incf index)
			(setf numerator (- code #.(char-code #\0)))
			(go perhaps-integer))
		       (;; Perhaps it is some other constituent
			(eq (syntax-type table char) 'constituent)
			(cond (;; Perhaps it is a minus sign
			       (has-constituent-trait-p table char +minus-sign+)
			       (setf sign -1)
			       (setf (schar buffer index) char)
			       (incf index)
			       (go perhaps-integer))
			      (;; Or perhaps a plus sign
			       (has-constituent-trait-p table char +plus-sign+)
			       (setf (schar buffer index) char)
			       (incf index)
			       (go perhaps-integer))
                              (;; or perhaps a package marker
                               (has-constituent-trait-p table char +package-marker+)
                               ;; We found a package marker.  Remember its position
                               ;; and change state to reflect our discovery.
                               (setf first-package-marker-position index)
			       (setf (schar buffer index) char)
			       (incf index)
                               (go symbol-even-escape-one-package-marker))
			      (;; It might be a dot
			       (has-constituent-trait-p table char +dot+)
			       ;; Save it
			       (setf (schar buffer index) (schar case-table code))
			       (incf index)
			       ;; Read the next character
			       (setf char (read-char input-stream nil nil t))
			       (when (null char)
				 ;; We have a consing dot at end of file
				 (error 'single-dot-token
					:stream input-stream))
			       ;; Come here if the dot is
			       ;; followed by some other character
			       (setf code (char-code char))
			       (ecase (syntax-type table char)
				 (whitespace
				    ;; handle read-preserving-whitespace
				    (error 'single-dot-token
					   :stream input-stream))
				 ((single-escape multiple-escape)
				    ;; We know this must be a symbol.
				    ;; Rather than repeating the code for
				    ;; such a case, go to the label that
				    ;; can handle that.  This means repeating
				    ;; a few tests.
				    (unread-char char input-stream)
				    (go symbol-even-escape-no-package-marker))
				 (terminating-macro-char
				    ;; Again we have a single dot.
				    (unread-char char input-stream)
				    (error 'single-dot-token
					   :stream input-stream))
				 (constituent
				    ;; This is the complicated case, because
				    ;; it can indeed be a dot followed by some
				    ;; constituent that makes it a symbol, but
				    ;; if the dot also has +decimal-point+
				    ;; constituent traits, and it is followed
				    ;; by a digit, then it might be a float.
				    (push-char char)
				    (if (and (has-constituent-trait-p 
					      table
					      (schar buffer (- index 2))
					      +decimal-point+)
					     (< (setf code (char-code char)) 128)
					     (= (sbit decimal-digits code) 1))
					;; It might be the beginning of a float.
					(progn (setf scale -1)
					       (setf numerator
						     (- code #.(char-code #\0)))
					       (go perhaps-float-with-decimal-point))
					;; It must be a symbol
					(go symbol-even-escape-no-package-marker)))))
			      (;; Not a dot, but perhaps a decimal point
			       (has-constituent-trait-p table char +dot+)
			       ;; Save it
			       (setf (schar buffer index) (schar case-table code))
			       (incf index)
			       ;; Read the next character
			       (setf char (read-char input-stream nil nil t))
			       (when (null char)
				 ;; Signal a more specific condition here
				 (error 'reader-error :stream input-stream))
			       ;; Not at end of file.  We might have
			       ;; a symbol or the beginning of a float.
			       (push-char char)
			       (cond ((and (< (setf code (char-code char)) 128)
					   (= (sbit decimal-digits code) 1))
				      ;; It might be the begginning of a float.
				      (setf scale -1)
				      (setf numerator
					    (= code #.(char-code #\0)))
				      (go perhaps-float-with-decimal-point))
				     ((eq (syntax-type table char) 'constituent)
				      ;; It must be a symbol
				      (go symbol-even-escape-no-package-marker))
				     (t
				      (unread-char char input-stream)
				      ;; signal a more specific condition here.
				      (error 'reader-error :stream input-stream))))))
		       (;; Or perhaps we have a single-escape
			(eq (syntax-type table char) 'single-escape)
			;; We do not accumulate single escapes.  Instead we read
			;; the following character and accumulate it.
			(setf char (read-char input-stream nil nil t))
			(if (null char)
			    (error 'reader-error :stream input-stream)
			    (progn
			      ;; There is no point in testing whether the buffer
			      ;; is full at this point, because we have only processed
			      ;; a single character.
			      (setf (schar buffer index) char)
			      (incf index)
			      (go symbol-even-escape-no-package-marker))))
		       (;; Or perhaps a multiple-escape
			(eq (syntax-type table char) 'multiple-escape)
			;; We do not accumulate multiple escapes either.
			;; We just remember the parity by going to the
			;; right state. 
			(go symbol-odd-escape-no-package-marker))
		       ((let ((type (syntax-type table char)))
			  (or (eq type 'terminating-macro-char)
			      (eq type 'non-terminating-macro-char)))
			(return-from read-upcase-downcase-preserve-decimal
                          (funcall (get-macro-character char)
                                   input-stream char)))
		       (t
			(error "don't know what this might be"))))
	       (error "can't get here a"))
	 symbol-even-escape-no-package-marker
	   ;; In this state, we are accumulating a token that must be
	   ;; a symbol.  We have seen an even number of multiple
	   ;; escape characters, so we must change the case of the
	   ;; letters we accumulate according to the current value of
	   ;; readtable-case.
	   ;; We have seen no package marker yet. 
	   ;; The variable char has already been accumulated, so is no
	   ;; longer useful.
	   (setf char (read-char input-stream nil nil t))
	   ;; Start by testing for end of file.
	   ;; Until we do, we can't do anthing else useful.
	   (when (not char)
	     ;; Found end of file. We have a complete symbol without
	     ;; any package markers.  Intern it in the current package, and
	     ;; return the result.
	     (return-from read-upcase-downcase-preserve-decimal
	       (intern (subseq buffer 0 index))))
	   ;; Come here if we did not have an end of file.
	   ;; The variable char contains the next character to process,
	   ;; but the variable code has not been set yet.
	   ;; We make a quick test to see if it is one of the character
	   ;; that absolutely has to be part of a symbol.  Notice that
	   ;; if this test fails, the character might still be a constituent,
	   ;; but the test for that case is more expensive, so we get rid of
	   ;; this important special case first.
	   (when (and (< (setf code (char-code char)) 128)
		      (= (sbit decimal-letters code) 1))
	     ;; Accumulate the symbol and stay in the same state. 
	     (push-char (schar case-table code))
	     (go symbol-even-escape-no-package-marker))
	   ;; We failed to detect a constituent the fast way.
	   ;; Try the more general way.
	   (ecase (syntax-type table char)
	     (constituent
		(cond ((has-constituent-trait-p table char +package-marker+)
		       ;; We found a package marker.  Remember its position
		       ;; and change state to reflect our discovery.
		       (setf first-package-marker-position index)
		       ;; Technically, we don't need to accumulate the package
		       ;; marker, but it could be nice for error reporting to
		       ;; have it part of the buffer. 
		       (push-char char)
		       (go symbol-even-escape-one-package-marker))
		      ((has-constituent-trait-p table char +invalid+)
		       ;; FIXME: Do this better by signaling a more
		       ;; specific condition containing the invalid character.
		       (error 'reader-error :stream input-stream))
		      (t
		       ;; We found an ordinary constituent.  Accumulate it and
		       ;; reenter the same state. 
		       (push-char (schar case-table code))
		       (go symbol-even-escape-no-package-marker))))
	     (non-terminating-macro-char
		;; It is non-terminating, so we consider it part of the
		;; symbol we are accumulating. 
		;; Here we cannot use the fast way of converting the
		;; case of the character, because we are not sure it
		;; has a code that is less than 128.
		(push-char (funcall case-function char))
		(go symbol-even-escape-no-package-marker))
	     (terminating-macro-char
		;; We have seen a complete symbol, and we must unread
		;; the macro character so that it can be read the next
		;; time read is called.
		(unread-char char input-stream)
		(return-from read-upcase-downcase-preserve-decimal
		  (intern (subseq buffer 0 index))))
	     (single-escape
		;; We do not accumulate single escapes.  Instead we read
		;; the following character and accumulate it.
		(setf char (read-char input-stream nil nil t))
		(if (null char)
		    (error 'reader-error :stream input-stream)
		    (progn (push-char char)
			   (go symbol-even-escape-no-package-marker))))
	     (multiple-escape
		;; We do not accumulate multiple escape characters.  
		;; We just remember the parity by going to the corresponding state. 
		(go symbol-odd-escape-no-package-marker))
	     (whitespace
		;; We have seen an even number of multiple escapes,
		;; so a space terminates the symbol.
		;; FIXME: handle `read-preserving-whitespace' here
		(unread-char char input-stream)
		(return-from read-upcase-downcase-preserve-decimal
		  (intern (subseq buffer 0 index)))))
	   ;; We can't come here because of the preceding ecase.
	 symbol-even-escape-one-package-marker
	   ;; In this state, we are accumulating a token that must be
	   ;; a symbol.  We have seen an even number of multiple
	   ;; escape characters, so we must adjust the case of the
	   ;; letters we accumulate according to the current value of
	   ;; readtable-case.
	   ;; We have seen one package marker. 
	   ;; The variable char has been processed, so it's value is useless now.
	   (setf char (read-char input-stream nil nil t))
	   ;; Start by testing for end of file.
	   ;; Until we do, we can't do anthing else useful.
	   (flet ((return-or-error ()
		    (let ((package (find-package
				    (if (= 0 first-package-marker-position)
                                        "KEYWORD"
                                        (subseq buffer 0 first-package-marker-position))))
                          (name (subseq buffer
                                        (1+ first-package-marker-position)
                                        index)))
		      (unless package
			(error "no package by that name exists"))
		      (multiple-value-bind (symbol status)
			  (find-symbol name package)
                        (unless symbol
                          (if (string= (package-name package) "KEYWORD")
                              (progn
                                (intern name package)
                                (multiple-value-setq (symbol status)
                                 (find-symbol name package)))
                              (error "symbol does not exists")))
			(unless (eq status :external)
			  (error "symbol is not external"))
			(return-from read-upcase-downcase-preserve-decimal
			  symbol)))))
	     (when (not char)
	       ;; Found end of file.
	       (return-or-error))
	     ;; Come here if we did not have an end of file.
	     ;; The variable char contains the next character to process,
	     ;; but the variable code has not been set yet.
	     ;; We make a quick test to see if it is one of the character
	     ;; that absolutely has to be part of a symbol.  Notice that
	     ;; if this test fails, the character might still be a constituent,
	     ;; but the test for that case is more expensive, so we get rid of
	     ;; this important special case first.
	     (when (and (< (setf code (char-code char)) 128)
			(= (sbit decimal-letters code) 1))
	       (push-char (schar case-table code))
	       (go symbol-even-escape-one-package-marker))
	     ;; We failed to detect a constituent the fast way.
	     ;; Try the more general way.
	     (ecase (syntax-type table char)
	       (constituent
		  (cond ((has-constituent-trait-p table char +package-marker+)
			 ;; We found a package marker.  Remember its position
			 ;; and change state to reflect our discovery.
			 (setf second-package-marker-position index)
			 ;; Technically, we don't need to accumulate
			 ;; the package marker, but it could be nice
			 ;; for error reporting to have it part of the
			 ;; buffer.
			 (push-char char)
			 (go symbol-even-escape-two-package-markers))
			((has-constituent-trait-p table char +invalid+)
			 ;; FIXME: Do this better by signaling a more
			 ;; specific condition containing the invalid
			 ;; character.
			 (error 'reader-error :stream input-stream))
			(t
			 ;; We found an ordinary constituent.
			 ;; Accumulate it and reenter the same state.
			 (push-char (schar case-table code))
			 (go symbol-even-escape-one-package-marker))))
	       (non-terminating-macro-char
		  ;; It is non-terminating, so we consider it part of the
		  ;; symbol we are accumulating. 
		  ;; Here we cannot use the fast way of converting the
		  ;; case of the character, because we are not sure it
		  ;; has a code that is less than 128.
		  (push-char (funcall case-function char))
		  (go symbol-even-escape-one-package-marker))
	       (terminating-macro-char
		  ;; We have seen a complete symbol, and we must unread
		  ;; the macro character so that it can be read the next
		  ;; time read is called.
		  (unread-char char input-stream)
		  (return-or-error))
	       (single-escape
		  ;; We do not accumulate single escapes.  Instead we read
		  ;; the following character and accumulate it.
		  (setf char (read-char input-stream nil nil t))
		  (if (null char)
		      (error 'reader-error :stream input-stream)
		      (progn
			(push-char char)
			(go symbol-even-escape-one-package-marker))))
	       (multiple-escape
		  ;; We do not accumulate multiple escape characters.  
		  ;; We just remember the parity by going to the corresponding state. 
		  (go symbol-odd-escape-one-package-marker))
	       (whitespace
		  ;; FIXME: handle `read-preserving-whitespace' here
		  (unread-char char input-stream)
		  (return-or-error))))
	   ;; We can't come here because of the preceding ecase.
	 symbol-even-escape-two-package-markers
	   ;; In this state, we are accumulating a token that must be
	   ;; a symbol.  We have seen an even number of multiple
	   ;; escape characters, so we must adjust the case of the
	   ;; letters we accumulate according to the value of
	   ;; readtable-case.
	   ;; We have seen two package markers already.
	   ;; The variable char has been processed, so it's value is
	   ;; useless now.
	   (setf char (read-char input-stream nil nil t))
	   ;; Start by testing for end of file.
	   ;; Until we do, we can't do anthing else useful.
	   (flet ((return-or-error ()
		    (unless (= (1+ first-package-marker-position)
			       second-package-marker-position)
		      (error "the two package markers are not adjacent"))
		    (when (= first-package-marker-position 1)
		      (error "cannot have two package markers at beginning"))
		    (when (= second-package-marker-position index)
		      (error "cannot have two package markers at the end"))
		    (let ((package (find-package
				    (subseq buffer 0 first-package-marker-position))))
		      (unless package
			(error "no package by that name exists"))
		      (return-from read-upcase-downcase-preserve-decimal
			(intern (subseq buffer
					(1+ second-package-marker-position)
					index)
				package)))))
	     (when (not char)
	       ;; Found end of file.
	       (return-or-error))
	     ;; We make a quick test to see if it is one of the character
	     ;; that absolutely has to be part of a symbol.  Notice that
	     ;; if this test fails, the character might still be a constituent,
	     ;; but the test for that case is more expensive, so we get rid of
	     ;; this important special case first.
	     (when (and (< (setf code (char-code char)) 128)
			(= (sbit decimal-letters code) 1))
	       (push-char (schar case-table code))
	       (go symbol-even-escape-two-package-markers))
	     ;; We failed to detect a constituent the fast way.
	     ;; Try the more general way.
	     (ecase (syntax-type table char)
	       (constituent
		  (cond ((has-constituent-trait-p table char +package-marker+)
			 ;; We found a package marker.  That's one too many.
			 (error "more than two package markers in a token"))
			((has-constituent-trait-p table char +invalid+)
			 ;; FIXME: Do this better
			 (error 'reader-error :stream input-stream))
			(t
			 ;; We found an ordinary constituent.  Save it and
			 ;; reenter the same state. 
			 (push-char (funcall case-function char))
			 (go symbol-even-escape-two-package-markers))))
	       (non-terminating-macro-char
		  (push-char (schar case-table code))
		  (go symbol-even-escape-two-package-markers))
	       (terminating-macro-char
		  (unread-char char input-stream)
		  (return-or-error))
	       (single-escape
		  (setf char (read-char input-stream nil nil t))
		  (if (null char)
		      (error 'reader-error :stream input-stream)
		      (progn (push-char char)
			     (go symbol-even-escape-two-package-markers))))
	       (multiple-escape
		  (go symbol-odd-escape-two-package-markers))
	       (whitespace
		  ;; FIXME: handle `read-preserving-whitespace' here
		  (unread-char char input-stream)
		  (return-or-error))))
	   ;; We can't come here because of the preceding ecase.
	 symbol-odd-escape-no-package-marker
	   ;; In this state, we are accumulating a token that must be
	   ;; a symbol.  We have seen an odd number of multiple escape
	   ;; characters, so we do not alter the case of the letters
	   ;; we accumulate.
	   (setf char (read-char input-stream nil nil t))
	   (when (null char)
	     (error 'reader-error :stream input-stream))
	   (ecase (syntax-type table char)
	     ((constituent non-terminating-macro-char terminating-macro-char whitespace)
		(push-char char)
		(go symbol-odd-escape-no-package-marker))
	     (single-escape
		(setf char (read-char input-stream nil nil t))
		(if (null char)
		    (error 'reader-error :stream input-stream)
		    (progn (push-char char)
			   (go symbol-odd-escape-no-package-marker))))
	     (multiple-escape
		(go symbol-even-escape-no-package-marker)))
	   ;; We can't come here because of the preceding ecase.
	 symbol-odd-escape-one-package-marker
	   ;; In this state, we are accumulating a token that must be
	   ;; a symbol.  We have seen an odd number of multiple escape
	   ;; characters, so we do not alter the case of the letters
	   ;; we accumulate.
	   (setf char (read-char input-stream nil nil t))
	   (when (null char)
	     (error 'reader-error :stream input-stream))
	   (ecase (syntax-type table char)
	     ((constituent non-terminating-macro-char terminating-macro-char whitespace)
		(push-char char)
		(go symbol-odd-escape-one-package-marker))
	     (single-escape
		(setf char (read-char input-stream nil nil t))
		(if (null char)
		    (error 'reader-error :stream input-stream)
		    (progn (push-char char)
			   (go symbol-odd-escape-one-package-marker))))
	     (multiple-escape
		(go symbol-even-escape-one-package-marker)))
	   ;; We can't come here because of the preceding ecase.
	 symbol-odd-escape-two-package-markers
	   ;; In this state, we are accumulating a token that must be
	   ;; a symbol.  We have seen an odd number of multiple escape
	   ;; characters, so we do not alter the case of the letters
	   ;; we accumulate.
	   (setf char (read-char input-stream nil nil t))
	   (when (null char)
	     (error 'reader-error :stream input-stream))
	   (ecase (syntax-type table char)
	     ((constituent non-terminating-macro-char terminating-macro-char whitespace)
		(push-char char)
		(go symbol-odd-escape-two-package-markers))
	     (single-escape
		(setf char (read-char input-stream nil nil t))
		(if (null char)
		    (error 'reader-error :stream input-stream)
		    (progn (push-char char)
			   (go symbol-odd-escape-two-package-markers))))
	     (multiple-escape
		(go symbol-even-escape-two-package-markers)))
	   ;; We can't come here because of the preceding ecase.
	 perhaps-integer
	   ;; We have seen the a sequence of digits, possibly preceded by
	   ;; as sign.  
	   (flet ((buffer-has-decimal-digit ()
                    (or (> numerator 0)
                        (some (lambda (c)
                                (and (< (setf c (char-code c)) 128)
                                     (= (sbit decimal-digits c) 1)))
                              (subseq buffer 0 index)))))
             (setf char (read-char input-stream nil nil t))
             ;; Start by testing for end of file.
             ;; Until we do, we can't do anthing else useful.
             (when (not char)
               ;; Found end of file.
               (return-from read-upcase-downcase-preserve-decimal
                 (if (buffer-has-decimal-digit)
                     (* sign numerator)
                     (intern (subseq buffer 0 index)))))
            (when (and (< (setf code (char-code char)) 128)
                       (= (sbit decimal-digits code) 1))
              (push-char char)
              (setf numerator (+ (* 10 numerator) (- code #.(char-code #\0))))
              (go perhaps-integer))
            (if (eq (syntax-type table char) 'constituent)
                (cond ((has-constituent-trait-p table char +ratio-marker+)
                       (push-char char)
                       (setf char (read-char input-stream nil nil t))
                       (cond ((null char)
                              (return-from read-upcase-downcase-preserve-decimal
                                (intern (subseq buffer 0 index))))
                             ((and (< (setf code (char-code char)) 128)
                                   (= (sbit decimal-digits code) 1))
                              (push-char char)
                              (setf denominator (- code #.(char-code #\0)))
                              (go perhaps-ratio))
                             (t
                              (push-char (funcall case-function char))
                              (go symbol-even-escape-no-package-marker))))
                      ((has-constituent-trait-p
                        table char +decimal-point+)
                       (push-char char)
                       ;; We could have a decimal integer, or we could
                       ;; have a floating-point number.  Check first for 
                       ;; integer. 
                       (setf char (read-char input-stream nil nil t))
                       ;; Start by testing for end of file.
                       ;; Until we do, we can't do anthing else useful.
                       (when (not char)
                         ;; Found end of file.
                         (return-from read-upcase-downcase-preserve-decimal
                           (if (buffer-has-decimal-digit)
                               (* sign numerator)
                               (intern (subseq buffer 0 index)))))
                       (when (and (< (setf code (char-code char)) 128)
                                  (= (sbit decimal-digits code) 1))
                         (push-char char)
                         (setf numerator (+ (* 10 numerator) (- code #.(char-code #\0))))
                         (setf scale -1)
                         (go perhaps-float-with-decimal-point))
                       (ecase (syntax-type table char)
                         (whitespace
                          (return-from read-upcase-downcase-preserve-decimal
                            (if (buffer-has-decimal-digit)
                                (* sign numerator)
                                (intern (subseq buffer 0 index)))))
                         ((constituent non-terminating-macro-char)
                          (push-char (funcall case-function char))
                          (go symbol-even-escape-no-package-marker))
                         (single-escape
                          (setf char (read-char input-stream nil nil t))
                          (if (null char)
                              (error 'reader-error :stream input-stream)
                              (progn (push-char (funcall case-function char))
                                     (go symbol-odd-escape-two-package-markers))))
                         (multiple-escape
                          (go symbol-even-escape-two-package-markers))
                         (terminating-macro-char
                          (unread-char char input-stream)
                          (return-from read-upcase-downcase-preserve-decimal
                            (if (buffer-has-decimal-digit)
                                (/ (* sign numerator) denominator)
                                (intern (subseq buffer 0 index)))))))
                      ((has-constituent-trait-p
                        table char +float-exponent-marker+)
                       (push-char (funcall case-function char))
                       (setf float-prototype float-prototype)
                       (go pre-perhaps-float-with-exponent-marker))
                      ((has-constituent-trait-p
                        table char +short-float-exponent-marker+)
                       (push-char (funcall case-function char))
                       (setf float-prototype 1s0)
                       (go pre-perhaps-float-with-exponent-marker))
                      ((has-constituent-trait-p
                        table char +single-float-exponent-marker+)
                       (push-char (funcall case-function char))
                       (setf float-prototype 1f0)
                       (go pre-perhaps-float-with-exponent-marker))
                      ((has-constituent-trait-p
                        table char +double-float-exponent-marker+)
                       (push-char (funcall case-function char))
                       (setf float-prototype 1d0)
                       (go pre-perhaps-float-with-exponent-marker))
                      ((has-constituent-trait-p
                        table char +long-float-exponent-marker+)
                       (push-char (funcall case-function char))
                       (setf float-prototype 1l0)
                       (go pre-perhaps-float-with-exponent-marker))
                      (t
                       (push-char (funcall case-function char))
                       (go symbol-even-escape-no-package-marker)))
                (progn (unread-char char input-stream)
                       (return-from read-upcase-downcase-preserve-decimal
                         (if (buffer-has-decimal-digit)
                             (* sign numerator)
                             (intern (subseq buffer 0 index)))))))
	   (error "can't come here either")
	 perhaps-ratio
	   ;; We have seen an integer, a slash, and at leat one more digit. 
	   (setf char (read-char input-stream nil nil t))
	   ;; Start by testing for end of file.
	   ;; Until we do, we can't do anthing else useful.
	   (when (not char)
	     ;; Found end of file.
	     (return-from read-upcase-downcase-preserve-decimal
	       (/ (* sign numerator) denominator)))
	   (when (and (< (setf code (char-code char)) 128)
		      (= (sbit decimal-digits code) 1))
	     (push-char char)
	     (setf denominator (+ (* 10 denominator) (- code #.(char-code #\0))))
	     (go perhaps-ratio))
	   (ecase (syntax-type table char)
	     (whitespace
		(return-from read-upcase-downcase-preserve-decimal
		  (/ (* sign numerator) denominator)))
	     ((constituent non-terminating-macro-char)
		(push-char (funcall case-function char))
		(go symbol-even-escape-no-package-marker))
	     (single-escape
		(setf char (read-char input-stream nil nil t))
		(if (null char)
		    (error 'reader-error :stream input-stream)
		    (progn (push-char (funcall case-function char))
			   (go symbol-odd-escape-two-package-markers))))
	     (multiple-escape
		(go symbol-even-escape-two-package-markers))
	     (terminating-macro-char
		(unread-char char input-stream)
		(return-from read-upcase-downcase-preserve-decimal
		  (/ (* sign numerator) denominator))))
	   ;; We can't come here because of the preceding ecase.
	 perhaps-float-with-decimal-point
	   ;; We have seen an integer followed by a decimal point
	   ;; followed by at least one more digit.
	   (setf char (read-char input-stream nil nil t))
	   ;; Start by testing for end of file.
	   ;; Until we do, we can't do anthing else useful.
	   (when (not char)
	     ;; Found end of file.
	     (return-from read-upcase-downcase-preserve-decimal
	       (float (decimal-to-float sign numerator scale) float-prototype)))
	   (when (and (< (setf code (char-code char)) 128)
		      (= (sbit decimal-digits code) 1))
	     (push-char char)
	     (setf numerator (+ (* 10 numerator) (- code #.(char-code #\0))))
	     (decf scale)
	     (go perhaps-float-with-decimal-point))
	   (if (eq (syntax-type table char) 'constituent)
	       (cond ((has-constituent-trait-p
		       table char +float-exponent-marker+)
		      (push-char (funcall case-function char))
		      (setf float-prototype float-prototype)
		      (go pre-perhaps-float-with-exponent-marker))
		     ((has-constituent-trait-p
		       table char +short-float-exponent-marker+)
		      (push-char (funcall case-function char))
		      (setf float-prototype 1s0)
		      (go pre-perhaps-float-with-exponent-marker))
		     ((has-constituent-trait-p
		       table char +single-float-exponent-marker+)
		      (push-char (funcall case-function char))
		      (setf float-prototype 1f0)
		      (go pre-perhaps-float-with-exponent-marker))
		     ((has-constituent-trait-p
		       table char +double-float-exponent-marker+)
		      (push-char (funcall case-function char))
		      (setf float-prototype 1d0)
		      (go pre-perhaps-float-with-exponent-marker))
		     ((has-constituent-trait-p
		       table char +long-float-exponent-marker+)
		      (push-char (funcall case-function char))
		      (setf float-prototype 1l0)
		      (go pre-perhaps-float-with-exponent-marker))
		     (t
		      (push-char (funcall case-function char))
		      (go symbol-even-escape-no-package-marker)))
	       (progn (unread-char char input-stream)
		      (return-from read-upcase-downcase-preserve-decimal
			(float (decimal-to-float sign numerator scale)
			       float-prototype))))
	   (ecase (syntax-type table char)
	     (whitespace
		(return-from read-upcase-downcase-preserve-decimal
		  (float (decimal-to-float sign numerator scale) float-prototype)))
	     (non-terminating-macro-char
		(push-char (funcall case-function char))
		(go symbol-even-escape-no-package-marker))
	     (single-escape
		(setf char (read-char input-stream nil nil t))
		(if (null char)
		    (error 'reader-error :stream input-stream)
		    (progn (push-char (funcall case-function char))
			   (go symbol-odd-escape-two-package-markers))))
	     (multiple-escape
		(go symbol-even-escape-two-package-markers))
	     (terminating-macro-char
		(unread-char char input-stream)
		(return-from read-upcase-downcase-preserve-decimal
		  (float (decimal-to-float sign numerator scale) float-prototype))))
	   ;; We can't come here because of the preceding ecase.
	 pre-perhaps-float-with-exponent-marker
	   ;; Come here when we have seen a mantissa followed by
	   ;; an exponent marker
	   (setf char (read-char input-stream nil nil t))
	   (when (null char)
	     ;; End of file.  We have a symbol
	     (return-from read-upcase-downcase-preserve-decimal
	       (intern (subseq buffer 0 index))))
	   (when (and (< (setf code (char-code char)) 128)
		      (= (sbit decimal-digits code) 1))
	     (push-char char)
	     (setf exponent (- code #.(char-code #\0)))
	     (go perhaps-float-with-exponent-marker))
	   (ecase (syntax-type table char)
	     (whitespace
		(return-from read-upcase-downcase-preserve-decimal
		  (intern (subseq buffer 0 index))))
	     ((constituent non-terminating-macro-char)
		(push-char (funcall case-function char))
		(go symbol-even-escape-no-package-marker))
	     (single-escape
		(setf char (read-char input-stream nil nil t))
		(if (null char)
		    (error 'reader-error :stream input-stream)
		    (progn (push-char (funcall case-function char))
			   (go symbol-odd-escape-two-package-markers))))
	     (multiple-escape
		(go symbol-even-escape-two-package-markers))
	     (terminating-macro-char
		(unread-char char input-stream)
		(return-from read-upcase-downcase-preserve-decimal
		  (intern (subseq buffer 0 index)))))
	   ;; We can't come here because of the preceding ecase.
	 perhaps-float-with-exponent-marker
	   ;; come here when we have seen a mantissa followed
	   ;; by an exponent marker followed by at least one digit
	   (setf char (read-char input-stream nil nil t))
	   (when (null char)
	     ;; End of file.  We have a float.
	     (return-from read-upcase-downcase-preserve-decimal
	       (float (decimal-to-float sign numerator (+ exponent scale))
		      float-prototype)))
	   (when (and (< (setf code (char-code char)) 128)
		      (= (sbit decimal-digits code) 1))
	     (push-char char)
	     (setf exponent (+ (* 10 exponent) (- code #.(char-code #\0))))
	     (go perhaps-float-with-exponent-marker))
	   (ecase (syntax-type table char)
	     (whitespace
		(return-from read-upcase-downcase-preserve-decimal
		  (float (decimal-to-float sign numerator (+ exponent scale))
			 float-prototype)))
	     ((constituent non-terminating-macro-char)
		(push-char (funcall case-function char))
		(go symbol-even-escape-no-package-marker))
	     (single-escape
		(setf char (read-char input-stream nil nil t))
		(if (null char)
		    (error 'reader-error :stream input-stream)
		    (progn (push-char (funcall case-function char))
			   (go symbol-odd-escape-two-package-markers))))
	     (multiple-escape
		(go symbol-even-escape-two-package-markers))
	     (terminating-macro-char
		(unread-char char input-stream)
		(return-from read-upcase-downcase-preserve-decimal
		  (float (decimal-to-float sign numerator (+ exponent scale))
			 float-prototype))))
	   ;; We can't come here because of the preceding ecase.
	   )))))

(defun read (&optional
	     input-stream
	     (eof-error-p t)
	     (eof-value nil)
	     (recursive-p nil))
  (setf input-stream
	(cond ((eq input-stream t) *terminal-io*)
	      ((null input-stream) *standard-input*)
	      (t input-stream)))
  ;;; Test for the read-base as well
  (case (readtable-case *readtable*)
    (:upcase
       (read-upcase-downcase-preserve-decimal
	input-stream eof-error-p eof-value recursive-p *ascii-upcase* #'char-upcase))
    (:downcase
       (read-upcase-downcase-preserve-decimal
	input-stream eof-error-p eof-value recursive-p *ascii-downcase* #'char-downcase))
    (:preserve
       (read-upcase-downcase-preserve-decimal
	input-stream eof-error-p eof-value recursive-p *ascii-preserve* #'identity))
    (:invert
       (error "can't handle :invert readcase yet"))))
  

(defun read-with-position (&optional
                           input-stream
                           (eof-error-p t)
                           (eof-value nil)
                           (recursive-p nil))
  (let ((*expression-stack* (list '())))
    (values (read input-stream eof-error-p eof-value recursive-p)
            (caar *expression-stack*))))


(defparameter *initial-readtable* (copy-readtable *standard-readtable*))

(setf *readtable* *initial-readtable*)

