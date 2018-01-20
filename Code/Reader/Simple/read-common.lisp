(cl:in-package #:sicl-reader)

(defgeneric call-reader-macro (function input-stream char))

(defmethod call-reader-macro (function input-stream char)
  (funcall (sicl-readtable:get-macro-character *readtable* char)
	   input-stream
	   char))

(defgeneric read-common (input-stream eof-error-p eof-value))

(defvar *stack*)

(defun create-cst (expression children source)
  (if (atom expression)
      (make-instance 'cst:atom-cst
        :raw expression
        :source source)
      (labels ((aux (expression)
                 (let ((cst (find expression children :key #'cst:raw)))
                   (if (null cst)
                       (if (atom expression)
                           (cst:cst-from-expression expression)
                           (make-instance 'cst:cons-cst
                             :raw expression
                             :first (aux (car expression))
                             :rest (aux (cdr expression))))
                       cst))))
        (make-instance 'cst:cons-cst
          :raw expression
          :first (aux (car expression))
          :rest (aux (cdr expression))
          :source source))))

(defgeneric source-position (stream client)
  (:method (stream client)
    (declare (ignore client))
    (file-position stream)))

(defmethod read-common :around (input-stream eof-error-p eof-value)
  (let ((*backquote-allowed-p* *backquote-in-subforms-allowed-p*)
	(*backquote-in-subforms-allowed-p* nil))
    (if (boundp '*stack*)
        (let ((*stack* (cons '() *stack*)))
          (loop for char = (read-char input-stream nil nil)
                when (null char)
                     do (if eof-error-p
                            (error 'end-of-file :stream input-stream)
                            (return-from read-common eof-value))
                while (eq (sicl-readtable:syntax-type *readtable* char)
                          :whitespace)
                finally (unread-char  char input-stream))
          (let* ((start (source-position input-stream *client*))
                 (result (call-next-method))
                 (end (source-position input-stream *client*))
                 (source (cons start end))
                 (cst (create-cst result (reverse (first *stack*)) source)))
            (push cst (second *stack*))
            result))
        (call-next-method))))

(defmethod read-common (input-stream eof-error-p eof-value)
  (tagbody
   step-1-start
     (let ((char (read-char input-stream nil nil)))
       (when (null char)
         (if eof-error-p
             (error 'end-of-file :stream input-stream)
             (return-from read-common eof-value)))
       (case (sicl-readtable:syntax-type *readtable* char)
         (:whitespace
          (go step-1-start))
         ((:terminating-macro :non-terminating-macro)
          (let ((values (multiple-value-list
                         (call-reader-macro
                          (sicl-readtable:get-macro-character *readtable* char)
                          input-stream
                          char))))
            (if (null values)
                (go step-1-start)
                (return-from read-common (car values)))))
         (t
          (unread-char char input-stream)
          (return-from read-common
            (read-token input-stream eof-error-p eof-value)))))))

(defgeneric read-token (input-stream eof-error-p eof-value))

(defmethod read-token (input-stream eof-error-p eof-value)
  (let ((token (make-array 100
			   :element-type 'character
			   :adjustable t
			   :fill-pointer 0))
	(token-escapes (make-array 100
				   :adjustable t
				   :fill-pointer 0)))
    (let ((char (read-char input-stream nil nil)))
      (tagbody
         (ecase (sicl-readtable:syntax-type *readtable* char)
           (:single-escape
            (let ((char (read-char input-stream nil nil)))
              (when (null char)
                (if eof-error-p
                    (error 'end-of-file :stream input-stream)
                    (return-from read-token eof-value)))
              (vector-push-extend char token)
              (vector-push-extend t token-escapes)
              (go step-8-even-escapes)))
           (:multiple-escape
            (go step-9-odd-escapes))
           (:constituent
            (vector-push-extend char token)
            (vector-push-extend nil token-escapes)
            (go step-8-even-escapes)))
       step-8-even-escapes
         (let ((char (read-char input-stream nil nil)))
           (when (null char)
             (go step-10-terminate-token))
           (ecase (sicl-readtable:syntax-type *readtable* char)
             ((:constituent :non-terminating-macro)
              (vector-push-extend char token)
              (vector-push-extend nil token-escapes)
              (go step-8-even-escapes))
             (:single-escape
              (let ((char (read-char input-stream nil nil)))
                (when (null char)
                  (if eof-error-p
                      (error 'end-of-file :stream input-stream)
                      (return-from read-token eof-value)))
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
                 (return-from read-token eof-value)))
           (ecase (sicl-readtable:syntax-type *readtable* char)
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
                      (return-from read-token eof-value)))
                (vector-push-extend char token)
                (vector-push-extend t token-escapes)
                (go step-9-odd-escapes)))
             (:multiple-escape
              (go step-8-even-escapes))))
       step-10-terminate-token
         (return-from read-token
           (if *read-suppress*
               nil
               (interpret-token token token-escapes input-stream)))))))
