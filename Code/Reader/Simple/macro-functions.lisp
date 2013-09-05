(cl:in-package #:sicl-reader)

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
	until (or (null char) (eql char #\Newline)))
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
  (list 'quote (read stream t nil t)))

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
	  do (when (eq (syntax-type char2) :single-escape)
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

(defparameter *backquote-allowed-p* t)

(defparameter *backquote-in-subforms-allowed-p* t)

(defparameter *backquote-depth* 0)

(defun backquote (stream char)
  (declare (ignore char))
  (unless *backquote-allowed-p*
    (error 'invalid-context-for-backquote
	   :stream stream))
  (let ((*backquote-depth* (1+ *backquote-depth*))
	(*backquote-in-subforms-allowed-p* *backquote-allowed-p*))
    `(quasiquote ,(read stream t nil t))))

(defun comma (stream char)
  (declare (ignore char))
  (unless *backquote-allowed-p*
    (error 'invalid-context-for-comma
	   :stream stream))
  (unless (plusp *backquote-depth*)
    (error 'comma-not-inside-backquote
	   :stream stream))
  (let* ((char2 (read-char stream t nil t))
	 (at-sign-p (if (eql char2 #\@)
			t
			(progn (unread-char char2 stream) nil)))
	 (*backquote-depth* (1- *backquote-depth*))
	 (*backquote-in-subforms-allowed-p* *backquote-allowed-p*))
    (let ((form (read stream t nil t)))
      (if at-sign-p
	  `(unquote-splicing ,form)
	  `(unquote ,form)))))

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

(defvar *consing-dot* '#:|.|)

(defparameter *consing-dot-allowed-p* nil)

(define-condition end-of-list () ())

(defvar *end-of-list* (make-condition 'end-of-list))

(defun left-parenthesis (stream char)
  (declare (ignore char))
  (let ((reversed-result '())
	(tail nil)
	(*consing-dot-allowed-p* t)
	(*backquote-in-subforms-allowed-p* *backquote-allowed-p*))
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
	  (nreconc reversed-result tail))))))

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
  `(function ,(read stream t nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign left parenthesis.

(defun sharpsign-left-parenthesis (stream char parameter)
  (declare (ignore char))
  (let ((*backquote-in-subforms-allowed-p* *backquote-allowed-p*))
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
      
