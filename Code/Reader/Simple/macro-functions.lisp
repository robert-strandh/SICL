(cl:in-package #:sicl-reader)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro WITH-PRESERVED-BACKQUOTE-CONTEXT.
;;;
;;; This macros allows backquotes in sub-forms if and only if
;;; backquotes are allowed in the main form.

(defmacro with-preserved-backquote-context (&body body)
  `(let ((*backquote-in-subforms-allowed-p* *backquote-allowed-p*))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for semicolon.
;;;
;;; We read characters until end-of-file or until we have read a
;;; newline character.  Since reading a comment does not generate an
;;; object, the semicolon reader must indicate that fact by returning
;;; zero values.

(defun semicolon (stream char)
  (declare (ignore char))
  (loop for char = (read-char stream nil nil t)
	until (or (null char) (eql char #\Newline))
        finally (when (eql char #\Newline)
                  (unread-char char stream )))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for single quote.
;;;
;;; They HyperSpec says that the reader signals an error if
;;; end-of-file is encountered before an object has been entirely
;;; parsed, independently of whether EOF-ERROR-P is true or not.  For
;;; that reason, we call the reader recursively with the value of
;;; EOF-ERROR-P being T.

(defun single-quote (stream char)
  (declare (ignore char))
  (with-preserved-backquote-context
    (list 'quote (read stream t nil t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for double quote.
;;;
;;; We identify a single escape character by its syntax type, so that
;;; if a user wants a different escape chacacter, we can handle that. 
;;;
;;; Furthermore, They HyperSpec says that the reader signals an error
;;; if end-of-file is encountered before an object has been entirely
;;; parsed, independently of whether EOF-ERROR-P is true or not.  For
;;; that reason, we call READ-CHAR with the value of EOF-ERROR-P being
;;; T.
;;;
;;; We accumulate characters in an adjustable vector.  However, the
;;; HyperSpec says that we must return a SIMPLE-STRING.  For that
;;; reason, we call COPY-SEQ in the end.  COPY-SEQ is guaranteed to
;;; return a simple vector.

(defun double-quote (stream char)
  (let ((result (make-array 100
			    :element-type 'character
			    :adjustable t
			    :fill-pointer 0)))
    (loop for char2 = (read-char stream t nil t)
	  until (eql char2 char)
	  do (when (eq (sicl-readtable:syntax-type *readtable* char2) :single-escape)
	       (setf char2 (read-char stream t nil t)))
	     (vector-push-extend char2 result))
    (copy-seq result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macros for backquote and comma.
;;;
;;; The control structure we use for backquote requires some
;;; explanation.  
;;;
;;; The HyperSpec says that backquote and comma are allowed only
;;; inside lists and vectors.  Since READ can be called recursively
;;; from other functions as well (such as the reader for arrays, or
;;; user-defined readers), we somehow need to detect whether we are
;;; about to read a list or a vector.  
;;;
;;; Perhaps the easiest way to do this would be to bind a flag to
;;; false in all readers EXCEPT the ones for lists and vectors.  This
;;; solution would require a programmer who writes a custom reader
;;; macro to do the same, or else backquote and comma would be
;;; processed in sub-forms.  Clearly, this solution is not so great.
;;;
;;; So we need a way for readers for lists and vectors to explicitly
;;; allow for backquote and comma, whereas BY DEFAULT, they should not
;;; be allowed.  We solve this by introducting two variables:
;;; *BACKQUOTE-ALLOWED-P* and *BACKQUOTE-IN-SUBFORMS-ALLOWED-P*.
;;; Initially the two are TRUE.  Whenever READ is called, it binds the
;;; variable *BACKQUOTE-ALLOWED-P* to the value of
;;; *BACKQUOTE-IN-SUBFORMS-ALLOWED-P*, and it binds
;;; *BACKQUOTE-IN-SUBFORMS-ALLOWED-P* to FALSE.  If no special action
;;; is taken, wheen READ is called recursively from a reader macro,
;;; the value of *BACKQUOTE-ALLOWED-P* will be FALSE.  When one of the
;;; reader macros left-parenthesis, sharpsign-left-parenthesis,
;;; backquote, or comma is called, before the recursive call to READ,
;;; *BACKQUOTE-IN-SUBFORMS-ALLOWED-P* is bound to the value of
;;; *BACKQUOTE-ALLOWED-P*.  Consequenctly, if the list or the vector
;;; is read in a context where backquote is not allowed, then it will
;;; not be allowed in subforms either, for instance if the list or the
;;; vector is inside an array.  But if the list or the vector is read
;;; in a context where backquote is allowed, then it will be allowed
;;; in subforms as well.
;;;
;;; The HyperSpec explicitly encourages us (see section 2.4.6.1) to
;;; follow the example of Scheme for representing backquote
;;; expression.  We see no reason for choosing a different
;;; representation, so we use (QUASIQUOTE <form>), (UNQUOTE <form>),
;;; and (UNQUOTE-SPLICING <form>).  Then we define QUASIQUOTE as a
;;; macro that expands to a CL form that will build the final data
;;; structure.

(defgeneric wrap-in-quasiquote (form client)
  (:method (form client)
    (declare (ignore client))
    `(quasiquote ,form)))

(defgeneric wrap-in-unquote (form client)
  (:method (form client)
    (declare (ignore client))
    `(unquote ,form)))

(defgeneric wrap-in-unquote-splicing (form client)
  (:method (form client)
    (declare (ignore client))
    `(unquote-splicing ,form)))

(defun backquote (stream char)
  (declare (ignore char))
  (unless *backquote-allowed-p*
    (error 'invalid-context-for-backquote
	   :stream stream))
  (let ((*backquote-depth* (1+ *backquote-depth*)))
    (with-preserved-backquote-context
      (wrap-in-quasiquote (read stream t nil t) *client*))))

(defun comma (stream char)
  (declare (ignore char))
  (unless (plusp *backquote-depth*)
    (error 'comma-not-inside-backquote
	   :stream stream))
  (let* ((char2 (read-char stream t nil t))
	 (at-sign-p (if (eql char2 #\@)
			t
			(progn (unread-char char2 stream) nil)))
	 (*backquote-depth* (1- *backquote-depth*)))
    (with-preserved-backquote-context
      (let ((form (read stream t nil t)))
	(if at-sign-p
            (wrap-in-unquote-splicing form *client*)
            (wrap-in-unquote form *client*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macros for left-parenthesis and right-parenthesis.
;;;
;;; The HyperSpec says that right-parenthesis is a macro character.
;;; In the reader macro for left-parenthesis, we can not just read
;;; until we find a righ parenthesis, because it is possible that some
;;; other character has been assigned the same meaning, and we need to
;;; handle that situation too.
;;;
;;; Another problem we need to solve is that of the CONSING-DOT.  The
;;; HyperSpec says that it is a token.  For that reason, we can not
;;; just read characters and look for a single period, because it is
;;; possible that the single dot has a different syntax type in this
;;; particular readtable.  Furthermore, we must handle error
;;; situations such as an attempt to use more than one dot in a list,
;;; or having zero or strictly more than one expression following a
;;; dot.
;;;
;;; We solve these problems as follows: the reader macro for a right
;;; parenthesis calls SIGNAL with a particular condition (of type
;;; END-OF-LIST).  In situations where the right parenthesis is
;;; allowed, there will be a handler for this condition type.
;;; Therefore, in that situation, the call to SIGNAL will not return.
;;; If the call to SIGNAL returns, we signal and ERROR, because then
;;; the right parenthesis was read in a context where it is not
;;; allowed.
;;;
;;; The reader macro for left parenthesis manages two local variables,
;;; REVERSED-RESULT and TAIL.  The variable REVERSED-RESULT is used to
;;; accumulate elements of the list (preceding a possible consing dot)
;;; being read, in reverse order.  A handler for END-OF-LIST is
;;; established around the recursive calls to READ inside the reader
;;; macro function.  When this handler is invoked, it calls NRECONC to
;;; reverse the value of REVERSED-RESULT and attach the value of TAIL
;;; to the end.  Normally, the value of TAIL is NIL, so the handler
;;; will create and return a proper list containing the accumulated
;;; elements.
;;;
;;; We use a special variable name *CONSING-DOT-ALLOWED-P* to
;;; determine the contexts in which a consing dot is allowed.
;;; Whenever the token parser detects a consing dot, it examines this
;;; variable, and if it is true it returns the unique CONSING-DOT
;;; token, and if it is false, signals an error.  Initially, this
;;; variable has the value FALSE.  Whenever the reader macro for left
;;; parenthesis is called, it binds this variable to TRUE.  When a
;;; recursive call to READ returns with the consing dot as a value,
;;; the reader macro for left parenthesis does three things.  First it
;;; SETS (as opposed to BINDS) *CONSING-DOT-ALLOWED-P* to FALSE, so
;;; that if a second consing dot should occur, then the token reader
;;; signals an error.  Second, it establishes a nested handler for
;;; END-OF-LIST, so that if a right parenthesis should occur
;;; immediately after the consing dot, then an error is signaled.
;;; With this handler established, READ is called.  If it returns
;;; normally, then the return value becomes the value of the variable
;;; TAIL.  Third, it calls READ again without any nested handler
;;; established.  This call had better result in a right parenthesis,
;;; so that END-OF-LIST is signaled, which is caught by the outermost
;;; handler and the correct list is built and returned.  If this call
;;; should return normally, we have a problem, because this means that
;;; there was a second subform after the consing dot in the list, so
;;; we signal an ERROR.

(defun left-parenthesis (stream char)
  (declare (ignore char))
  (let ((reversed-result '())
	(tail nil)
	(*consing-dot-allowed-p* t))
    (with-preserved-backquote-context
      (handler-case
	  (loop for object = (read stream t nil t)
		do (if (eq object *consing-dot*)
		       (progn (setf *consing-dot-allowed-p* nil)
			      (handler-case
				  (setf tail (read stream t nil t))
				(end-of-list ()
				  (error 'consing-dot-most-be-followed-by-object
					 :stream stream)))
			      ;; This call to read must not succeed.
			      (read stream t nil t)
			      (error 'multiple-objects-following-consing-dot
				     :stream stream))
		       (push object reversed-result)))
	(end-of-list ()
	  (return-from left-parenthesis
	    (nreconc reversed-result tail)))))))

(defun right-parenthesis (stream char)
  (declare (ignore char))
  ;; If the call to SIGNAL returns, then there is no handler for this
  ;; condition, which means that the right parenthesis was found in a
  ;; context where it is not allowed.
  (signal *end-of-list*)
  (error 'invalid-context-for-right-parenthesis
	 :stream stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign single quote.

(defun sharpsign-single-quote (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (warn 'numeric-parameter-supplied-but-ignored
	  :parameter parameter
	  :macro-name 'sharpsign-single-quote))
  (with-preserved-backquote-context
    `(function ,(read stream t nil t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign left parenthesis.

(defun sharpsign-left-parenthesis (stream char parameter)
  (declare (ignore char))
  (with-preserved-backquote-context
    (if (null parameter)
	(let ((reversed-elements '()))
	  (handler-case
	      (loop for object = (read stream t nil t)
		    do (push object reversed-elements))
	    (end-of-list ()
	      (return-from sharpsign-left-parenthesis
		(coerce (nreverse reversed-elements) 'simple-vector)))))
	(let ((result (make-array parameter))
	      (index 0))
	  (handler-case
	      (progn 
		(loop until (= index parameter)
		      for object = (read stream t nil t)
		      do (setf (aref result index) object)
			 (incf index))
		;; Read the closing right parenthesis
		(read stream t nil t)
		;; If we come here, then there were more objects
		;; than specified by the parameter.
		(warn 'extraneous-objects-ignored
		      :parameter parameter
		      :macro-name 'sharpsign-left-parenthesis)
		;; Read until the handler is invoked.
		(loop do (read stream t nil t)))
	    (end-of-list ()
	      ;; Come here when a closing parenthesis was found.
	      (unless (= index parameter)
		(if (zerop index)
		    ;; No objects were supplied, but the parameter given
		    ;; was greater than zero.
		    (warn 'no-objects-supplied
			  :parameter parameter
			  :macro-name 'sharpsign-left-parenthesis)
		    ;; Duplicate the last object supplied.
		    (loop for i from index below parameter
			  do (setf (aref result i) (aref result (1- index))))))
	      (return-from sharpsign-left-parenthesis result)))))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign dot.

(defun sharpsign-dot (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (warn 'numeric-parameter-supplied-but-ignored
	  :parameter parameter
	  :macro-name 'sharpsign-dot))
  (with-preserved-backquote-context
    (eval (read stream t nil t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign backslash.

(defparameter *character-names*
  (let ((table (make-hash-table :test 'equal)))
    (loop for (name . char) in '(("NEWLINE" .   #.(code-char 10))
				 ("SPACE" .     #.(code-char 32))
				 ("RUBOUT" .    #.(code-char 127))
				 ("PAGE" .      #.(code-char 12))
				 ("TAB" .       #.(code-char 9))
				 ("BACKSPACE" . #.(code-char 8))
				 ("RETURN" .    #.(code-char 13))
				 ("LINEFEED" .  #.(code-char 10)))
	  do (setf (gethash name table) char))
    table))

(defun sharpsign-backslash (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (warn 'numeric-parameter-supplied-but-ignored
	  :parameter parameter
	  :macro-name 'sharpsign-backslash))
  (let ((char1 (read-char stream nil nil t)))
    (when (null char1)
      (error 'end-of-file :stream stream))
    (let ((char2 (read-char stream nil nil t)))
      (cond ((null char2)
	     char1)
	    ((not (eq (sicl-readtable:syntax-type *readtable* char2) :constituent))
	     (unread-char char2 stream)
	     char1)
	    (t
	     (let ((token (make-array 10
				      :element-type 'character
				      :adjustable t
				      :fill-pointer 0)))
	       (vector-push-extend char1 token)
	       (vector-push-extend char2 token)
	       (tagbody
		even-escapes
		  (let ((char (read-char stream nil nil t)))
		    (when (null char)
		      (go terminate))
		    (ecase (sicl-readtable:syntax-type *readtable* char)
		      ((:constituent :non-terminating-macro)
		       (vector-push-extend char token)
		       (go even-escapes))
		      (:single-escape
		       (let ((char (read-char stream nil nil t)))
			 (when (null char)
			   (error 'end-of-file :stream stream))
			 (vector-push-extend char token))
		       (go even-escapes))
		      (:multiple-escape
		       (go odd-escapes))
		      (:terminating-macro
		       (unread-char char stream)
		       (go terminate))
		      (:whitespace
		       (when *preserve-whitespace*
			 (unread-char char stream))
		       (go terminate))))
		odd-escapes
		  (let ((char (read-char stream nil nil t)))
		    (when (null char)
		      (error 'end-of-file :stream stream))
		    (ecase (sicl-readtable:syntax-type *readtable* char)
		      ((:constituent :terminating-macro
			:non-terminating-macro :whitespace)
		       (vector-push-extend char token)
		       (go odd-escapes))
		      (:single-escape
		       (let ((char (read-char stream nil nil t)))
			 (when (null char)
			   (error 'end-of-file :stream stream))
			 (vector-push-extend char token))
		       (go odd-escapes))
		      (:multiple-escape
		       (go even-escapes))))
		terminate
		  (let* ((upcase (string-upcase token))
                         (char (gethash upcase *character-names*)))
		    (if (null char)
			(error 'unknown-character-name
			       :stream stream
			       :name token)
			(return-from sharpsign-backslash char))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign B, X, and O.

(defun read-rational (stream base)
  (let ((numerator 0)
	(denominator 0))
    (tagbody
     start
       (let ((char (read-char stream t nil t)))
	 (ecase (sicl-readtable:syntax-type *readtable* char)
	   ((:whitespace :terminating-macro
	     :non-terminating-macro :single-escape :multiple-escape)
	    (error 'digit-expected
		   :character-found char
		   :base base))
	   (:constituent
	    (unless (digit-char-p char base)
	      (error 'digit-expected
		     :character-found char
		     :base base))
	    (setf numerator
		  (+ (* base numerator) (digit-char-p char base)))
	    (go numerator))))
     numerator
       (let ((char (read-char stream nil nil t)))
	 (when (null char)
	   (return-from read-rational numerator))
	 (ecase (sicl-readtable:syntax-type *readtable* char)
	   (:whitespace
	    (when *preserve-whitespace*
	      (unread-char char stream))
	    (return-from read-rational numerator))
	   (:terminating-macro
	    (unread-char char stream)
	    (return-from read-rational numerator))
	   ((:non-terminating-macro :single-escape :multiple-escape)
	    (error 'digit-expected
		   :character-found char
		   :base base))
	   (:constituent
	    (when (eql char #\/)
	      (go denominator-start))
	    (unless (digit-char-p char base)
	      (error 'digit-expected
		     :character-found char
		     :base base))
	    (setf numerator
		  (+ (* base numerator) (digit-char-p char base)))
	    (go numerator))))
     denominator-start
       (let ((char (read-char stream t nil t)))
	 (ecase (sicl-readtable:syntax-type *readtable* char)
	   ((:whitespace :terminating-macro
	     :non-terminating-macro :single-escape :multiple-escape)
	    (error 'digit-expected
		   :character-found char
		   :base base))
	   (:constituent
	    (unless (digit-char-p char base)
	      (error 'digit-expected
		     :character-found char
		     :base base))
	    (setf denominator
		  (+ (* base denominator) (digit-char-p char base)))
	    (go denominator))))
     denominator
       (let ((char (read-char stream nil nil t)))
	 (when (null char)
	   (return-from read-rational (/ numerator denominator)))
	 (ecase (sicl-readtable:syntax-type *readtable* char)
	   (:whitespace
	    (when *preserve-whitespace*
	      (unread-char char stream))
	    (return-from read-rational (/ numerator denominator)))
	   (:terminating-macro
	    (unread-char char stream)
	    (return-from read-rational (/ numerator denominator)))
	   ((:non-terminating-macro :single-escape :multiple-escape)
	    (error 'digit-expected
		   :character-found char
		   :base base))
	   (:constituent
	    (unless (digit-char-p char base)
	      (error 'digit-expected
		     :character-found char
		     :base base))
	    (setf denominator
		  (+ (* base denominator) (digit-char-p char base)))
	    (go denominator)))))))

(defun sharpsign-b (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (warn 'numeric-parameter-supplied-but-ignored
	  :parameter parameter
	  :macro-name 'sharpsign-b))
  (read-rational stream 2.))
	   
(defun sharpsign-x (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (warn 'numeric-parameter-supplied-but-ignored
	  :parameter parameter
	  :macro-name 'sharpsign-x))
  (read-rational stream 16.))
	   
(defun sharpsign-o (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (warn 'numeric-parameter-supplied-but-ignored
	  :parameter parameter
	  :macro-name 'sharpsign-o))
  (read-rational stream 8.))
	   
(defun sharpsign-r (stream char parameter)
  (declare (ignore char))
  (unless (<= 2 parameter 36)
    (error 'invalid-radix
	   :stream stream
	   :radix parameter))
  (read-rational stream parameter))

(defun sharpsign-asterisk (stream char parameter)
  (declare (ignore char))
  (if (null parameter)
      (let ((v (make-array 10 :element-type 'bit :adjustable t :fill-pointer 0))
	    (illegal-character-p nil))
	(loop for char = (read-char stream nil nil t)
	      for syntax-type = (sicl-readtable:syntax-type *readtable* char)
	      for value = (digit-char-p char)
	      until (or (null char)
			(eq syntax-type :terminating-macro)
			(eq syntax-type :whitespace))
	      do (if (null value)
		     (setf illegal-character-p char)
		     (vector-push-extend value v)))
	(cond (*read-suppress*
	       nil)
	      (illegal-character-p
	       (error 'digit-expected
		      :stream stream
		      :character-found illegal-character-p
		      :base 2.))
	      (t
	       (coerce v 'simple-bit-vector))))
      (let ((result (make-array parameter :element-type 'bit))
	    (index 0)
	    (illegal-character-p nil)
	    (too-many-bits-p nil))
	(loop for char = (read-char stream nil nil t)
	      for syntax-type = (sicl-readtable:syntax-type *readtable* char)
	      for value = (digit-char-p char)
	      until (or (null char)
			(eq syntax-type :terminating-macro)
			(eq syntax-type :whitespace))
	      do (cond ((null value)
			(setf illegal-character-p char))
		       ((>= index parameter)
			(setf too-many-bits-p t))
		       (t
			(setf (sbit result index) value)))
		 (incf index))
	(cond (*read-suppress*
	       nil)
	      (illegal-character-p
	       (error 'digit-expected
		      :stream stream
		      :character-found illegal-character-p
		      :base 2.))
	      (too-many-bits-p
	       (error 'too-many-elements
		      :stream stream
		      :expected-number parameter
		      :number-found index))
	      ((zerop index)
	       (error 'no-elements-found
		      :stream stream
		      :expected-number parameter))
	      (t
	       (loop for i from index below parameter
		     do (setf (sbit result i) (sbit result (1- index))))
	       result)))))

(defun sharpsign-vertical-bar (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (warn 'numeric-parameter-supplied-but-ignored
	  :parameter parameter
	  :macro-name 'sharpsign-vertical-bar))
  (loop for char = (read-char stream t nil t)
	do (cond ((eql char #\#)
		  (let ((char2 (read-char stream t nil t)))
		    (if (eql char2 #\|)
			(sharpsign-vertical-bar stream #\| nil)
			(unread-char char2 stream))))
		 ((eql char #\|)
		  (let ((char2 (read-char stream t nil t)))
		    (if (eql char2 #\#)
			(return-from sharpsign-vertical-bar (values))
			(unread-char char2 stream))))
		 (t
		  nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign A.

(defun determine-dimensions (rank initial-contents)
  (cond ((zerop rank)
	 '())
	((not (or (and (listp initial-contents)
		       (cleavir-code-utilities:proper-list-p initial-contents))
		  (typep initial-contents 'sequence)))
	 (error 'type-error
		:expected-type 'sequence
		:datum initial-contents))
	(t
	 (let ((length (length initial-contents)))
	   (if (zerop length)
	       (make-list rank :initial-element 0)
	       (cons length (determine-dimensions
			     (1- rank) (elt initial-contents 0))))))))

(defun check-dimensions (dimensions initial-contents)
  (cond ((null dimensions)
	 t)
	((zerop (car dimensions))
	 (or (null initial-contents)
	     (and (typep initial-contents 'sequence)
		  (zerop (length initial-contents)))))
	((not (or (and (listp initial-contents)
		       (cleavir-code-utilities:proper-list-p initial-contents))
		  (typep initial-contents 'sequence)))
	 (error 'type-error
		:expected-type 'sequence
		:datum initial-contents))
	((/= (length initial-contents) (car dimensions))
	 (error 'incorrect-initialization-length
		:datum initial-contents
		:expected-length (car dimensions)))
	(t
	 (every (lambda (subseq)
		  (check-dimensions (cdr dimensions) subseq))
		initial-contents))))
	 
(defun sharpsign-a (stream char parameter)
  (declare (ignore char))
  (let ((init (read stream t nil t)))
    (let ((dimensions (determine-dimensions parameter init)))
      (check-dimensions dimensions init)
      (make-array dimensions :initial-contents init))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign colon.

(defun symbol-from-token (token token-escapes)
  (when *read-suppress*
    (return-from symbol-from-token nil))
  (convert-according-to-readtable-case token token-escapes)
  (make-symbol (copy-seq token)))
      

(defun sharpsign-colon (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (warn 'numeric-parameter-supplied-but-ignored
	  :parameter parameter
	  :macro-name 'sharpsign-colon))
  (let ((token (make-array 10
			   :element-type 'character
			   :adjustable t
			   :fill-pointer 0))
	(token-escapes (make-array 10
				   :adjustable t
				   :fill-pointer 0)))
    (tagbody
     even-escapes
       (let ((char (read-char stream nil nil t)))
	 (if (null char)
	     (return-from sharpsign-colon
	       (symbol-from-token token token-escapes))
	     (ecase (sicl-readtable:syntax-type *readtable* char)
	       (:whitespace
		(when *preserve-whitespace*
		  (unread-char char stream))
		(return-from sharpsign-colon
		  (symbol-from-token token token-escapes)))
	       (:terminating-macro
		(unread-char char stream)
		(return-from sharpsign-colon
		  (symbol-from-token token token-escapes)))
	       (:single-escape
		(let ((char2 (read-char stream t nil t)))
		  (vector-push-extend char2 token)
		  (vector-push-extend t token-escapes))
		(go even-escapes))
	       (:multiple-escape
		(go odd-escapes))
	       ((:constituent :non-terminating-macro)
		(vector-push-extend char token)
		(vector-push-extend nil token-escapes)
		(go even-escapes)))))
     odd-escapes
       (let ((char (read-char stream t nil t)))
	 (case (sicl-readtable:syntax-type *readtable* char)
	   (:single-escape
	    (let ((char2 (read-char stream t nil t)))
	      (vector-push-extend char2 token)
	      (vector-push-extend t token-escapes)
	      (go odd-escapes)))
	   (:multiple-escape
	    (go even-escapes))
	   (t
	    (vector-push-extend char token)
	    (vector-push-extend t token-escapes)
	    (go odd-escapes)))))))	    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign C.

(defun sharpsign-c (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (warn 'numeric-parameter-supplied-but-ignored
	  :parameter parameter
	  :macro-name 'sharpsign-c))
  (apply #'complex (read stream t nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign P.

(defun sharpsign-p (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (warn 'numeric-parameter-supplied-but-ignored
	  :parameter parameter
	  :macro-name 'sharpsign-p))
  (let ((expression (read stream t nil t)))
    (unless (stringp expression)
      (error 'type-error
	     :expected-type 'string
	     :datum expression))
    (parse-namestring expression)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macros for sharpsign + and sharpsign -.

(defun check-feature-expression (feature-expression)
  (unless (or (symbolp feature-expression)
	      (and (cleavir-code-utilities:proper-list-p feature-expression)
		   (consp feature-expression)))
    (error 'type-error
	   :datum feature-expression
	   :expected-type '(or symbol cons)))
  (when (consp feature-expression)
    (unless (member (car feature-expression) '(:not :or :and))
      (error 'type-error
	     :datum (car feature-expression)
	     :expected-type '(member :not :or :and)))
    (when (eq (car feature-expression) :not)
      (unless (null (cddr feature-expression))
	(error 'single-feature-expected
	       :features (cdr feature-expression))))
    (loop for feature in (cdr feature-expression)
	  do (unless (symbolp feature)
	       (error 'type-error
		      :datum feature
		      :expected-type 'symbol)))))

(defun evaluate-feature-expression (feature-expression)
  (if (symbolp feature-expression)
      (member feature-expression *features* :test #'eq)
      (ecase (car feature-expression)
	(:not
	 (evaluate-feature-expression (cadr feature-expression)))
	(:or
	 (some #'evaluate-feature-expression (cdr feature-expression)))
	(:and
	 (every #'evaluate-feature-expression (cdr feature-expression))))))

(defun sharpsign-plus (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (warn 'numeric-parameter-supplied-but-ignored
	  :parameter parameter
	  :macro-name 'sharpsign-p))
  (let* ((*package* (find-package '#:keyword))
	 (feature-expression (read stream t nil t)))
    (check-feature-expression feature-expression)
    (if (evaluate-feature-expression feature-expression)
	(read stream t nil t)
	(let ((*read-suppress* t))
	  (read stream t nil t)
	  (values)))))

(defun sharpsign-minus (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (warn 'numeric-parameter-supplied-but-ignored
	  :parameter parameter
	  :macro-name 'sharpsign-p))
  (let* ((*package* (find-package '#:keyword))
	 (feature-expression (read stream t nil t)))
    (check-feature-expression feature-expression)
    (if (evaluate-feature-expression feature-expression)
	(let ((*read-suppress* t))
	  (read stream t nil t)
	  (values))
	(read stream t nil t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macros for sharpsign < and sharpsign )

(defun sharpsign-invalid (stream char parameter)
  (declare (ignore parameter))
  (error 'sharpsign-invalid
	 :stream stream
	 :character-found char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macros for sharpsign equals.

(defun sharpsign-equals (stream char parameter)
  (declare (ignore char))
  (cond ((null parameter)
         ;; FIXME: define this error condition
         (error 'sharpsign-sharpsign-must-have-parameter))
        ((nth-value 1 (gethash parameter *labels*))
         ;; FIXME: define this error condition
         (error 'sharpsign-sharpsign-label-defined-more-than-once))
        (t
         (let ((contents (cons (list nil) nil)))
           (setf (gethash parameter *labels*) contents)
           ;; Hmm, do we need to transmit EOF-ERROR-P through reader
           ;; macros?
           (let ((result (read stream t nil t)))
             (setf (cdr contents) result)
             (setf (caar contents) t)
             result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macros for sharpsign sharpsign.

(defun sharpsign-sharpsign (stream char parameter)
  (declare (ignore char stream))
  (cond ((null parameter)
         ;; FIXME: define this error condition
         (error 'sharpsign-equals-must-have-parameter))
        ((not (nth-value 1 (gethash parameter *labels*)))
         ;; FIXME: define this error condition
         (error 'sharpsign-equals-undefined-label))
        (t
         (let ((contents (gethash parameter *labels*)))
           (if (caar contents)
               ;; Then the object in the CDR is the final one, so use
               ;; it.
               (cdr contents)
               ;; Else, the CAR is a temporary object that we must
               ;; use.
               (car contents))))))
