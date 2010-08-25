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

(in-package #:sicl-read)

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

(define-condition unmatched-right-parenthesis (reader-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Unmatched right parenthesis found."))))

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
     (format stream "A token consisting of a single dot was found ~@
                     in a context that does not permit such a token."))))

(define-condition no-object-preceding-dot (reader-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "A left parenthesis cannot be ~
                     immediately followed by a dot"))))

(define-condition multiple-objects-following-dot (reader-error)
  ((%offending-expression
    :initarg :offending-expression
    :reader offending-expression))
  (:report
   (lambda (condition stream)
     (format stream "A second expression following a dot~@
                     inside a list was found: ~S."
	     (offending-expression condition)))))

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
	       (single-dot-token () (error 'no-object-preceding-dot))))
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

(define-condition no-parameter-allowed (reader-error)
  ((%which-directive :initarg :which-directive :reader which-directive)
   (%parameter :initarg :parameter :reader parameter))
  (:report
   (lambda (condition stream)
     (format stream
	     "The ~a directive does not take a numeric parameter"
	     (which-directive condition)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sharpsign single quote

(defun sharpsign-single-quote-function (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (error 'no-parameter-allowed
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

(define-condition unknown-character-name (reader-error)
  ((%name :initarg :name :reader name)))

(defun sharpsign-backslash-function (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (error 'no-parameter-allowed
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
  (loop for key being each hash-key of (others from-readtable)
	using (hash-value value)
	do (setf (gethash key (others to-readtable)) value))
  (loop for key being each hash-key of (macro-functions from-readtable)
	using (hash-value value)
	do (setf (gethash key (macro-functions to-readtable)) value))
  to-readtable)

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

(defparameter *standard-readtable*
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
	char syntax-type token start)
    (tagbody
     1
       (setf start (file-position input-stream))
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
              (push-expression-stack)
              (unwind-protect 
                   (let* ((fun (gethash char (macro-functions table)))
                          (values (multiple-value-list
                                      (funcall fun input-stream char))))
                     (if (null values)
                         (go 1)
                         (progn (combine-expression-stack
                                 (first values) start (file-position input-stream))
                                (return-from read (first values)))))
                (pop-expression-stack)))
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
       (if (equal token ".")
           (error 'single-dot-token)
           ;; build the token here
           nil
           ))
    (push-expression-stack)
    (combine-expression-stack token start (file-position input-stream))
    (pop-expression-stack)
    token))

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

