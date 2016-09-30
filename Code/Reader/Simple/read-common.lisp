(cl:in-package #:sicl-reader)

(defgeneric call-reader-macro (function input-stream char))

(defmethod call-reader-macro (function input-stream char)
  (funcall (get-macro-character char)
	   input-stream
	   char))

(defun read-common (&optional
		      (input-stream *standard-input*)
		      (eof-error-p t)
		      (eof-value nil))
  (let ((token (make-array 100
			   :element-type 'character
			   :adjustable t
			   :fill-pointer 0))
	(token-escapes (make-array 100
				   :adjustable t
				   :fill-pointer 0))
	(*backquote-allowed-p* *backquote-in-subforms-allowed-p*)
	(*backquote-in-subforms-allowed-p* nil))
    (tagbody
     step-1-start
       (let ((char (read-char input-stream nil nil)))
	 (when (null char)
	   (if eof-error-p
	       (error 'end-of-file :stream input-stream)
	       (return-from read-common eof-value)))
	 (ecase (syntax-type char)
	   (:whitespace
	    (go step-1-start))
	   ((:terminating-macro :non-terminating-macro)
	    (let ((values (multiple-value-list
			   (call-reader-macro (get-macro-character char)
					      input-stream
					      char))))
	      (if (null values)
		  (go step-1-start)
		  (return-from read-common (car values)))))
	   (:single-escape
	    (let ((char (read-char input-stream nil nil)))
	      (when (null char)
		(if eof-error-p
		    (error 'end-of-file :stream input-stream)
		    (return-from read-common eof-value)))
	      (vector-push-extend char token)
	      (vector-push-extend t token-escapes)
	      (go step-8-even-escapes)))
	   (:multiple-escape
	    (go step-9-odd-escapes))
	   (:constituent
	    (vector-push-extend char token)
	    (vector-push-extend nil token-escapes)
	    (go step-8-even-escapes))))
     step-8-even-escapes
       (let ((char (read-char input-stream nil nil)))
	 (when (null char)
	   (go step-10-terminate-token))
	 (ecase (syntax-type char)
	   ((:constituent :non-terminating-macro)
	    (vector-push-extend char token)
	    (vector-push-extend nil token-escapes)
	    (go step-8-even-escapes))
	   (:single-escape
	    (let ((char (read-char input-stream nil nil)))
	      (when (null char)
		(if eof-error-p
		    (error 'end-of-file :stream input-stream)
		    (return-from read-common eof-value)))
	      (vector-push-extend char token)
	      (vector-push-extend t token-escapes)
	      (go step-8-even-escapes)))
	   (:multiple-escape
	    (go step-9-odd-escapes))
	   (:terminating-macro
	    (unread-char char input-stream)
	    (go step-10-terminate-token))
	   (:whitespace
	    (when *preserve-whitespace*
	      (unread-char char input-stream))
	    (go step-10-terminate-token))))
     step-9-odd-escapes
       (let ((char (read-char input-stream nil nil)))
	 (when (null char)
	   (if eof-error-p
	       (error 'end-of-file :stream input-stream)
	       (return-from read-common eof-value)))
	 (ecase (syntax-type char)
	   ((:constituent :terminating-macro
	     :non-terminating-macro :whitespace)
	    (vector-push-extend char token)
	    (vector-push-extend t token-escapes)
	    (go step-9-odd-escapes))
	   (:single-escape
	    (let ((char (read-char input-stream nil nil)))
	      (when (null char)
		(if eof-error-p
		    (error 'end-of-file :stream input-stream)
		    (return-from read-common eof-value)))
	      (vector-push-extend char token)
	      (vector-push-extend t token-escapes)
	      (go step-9-odd-escapes)))
	   (:multiple-escape
	    (go step-8-even-escapes))))
     step-10-terminate-token
       (return-from read-common
	 (if *read-suppress*
	     nil
	     (interpret-token token token-escapes input-stream))))))
