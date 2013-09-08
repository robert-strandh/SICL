(cl:in-package #:sicl-reader)

(defclass readtable ()
  ((%syntax-types
    :initform (make-hash-table)
    :reader syntax-types)
   (%macro-characters
    :initform (make-hash-table)
    :reader macro-characters)
   (%dispatch-macro-characters
    :initform (make-hash-table)
    :reader dispatch-macro-characters)
   (%readtable-case :initform :upcase :accessor readtable-case)))

(defun syntax-type (char)
  (or (gethash char (syntax-types *readtable*))
      :constituent))

(defun set-macro-character
    (char function &optional (non-terminating-p nil) (readtable *readtable*))
  (setf (gethash char (syntax-types readtable))
	(if non-terminating-p
	    :non-terminating-macro
	    :terminating-macro))
  (setf (gethash char (macro-characters readtable))
	function)
  t)

(defun get-macro-character (char &optional (readtable *readtable*))
  (let ((entry (gethash char (macro-characters readtable))))
    (values
     entry
     (eq (gethash char (syntax-types readtable)) :non-terminating-macro))))

(defun sharpsign (stream char)
  (loop for parameter = 0 then (+ (* 10 parameter) value)
	for parameter-given = nil then t
	for char2 = (read-char stream t nil t)
	for value = (digit-char-p char2)
	until (null value)
	finally (when (eq (syntax-type char2) :whitespace)
		  (error 'sharpsign-invalid
			 :stream stream
			 :character-found char2))
		(let ((fun (get-dispatch-macro-character char char2)))
		  (when (null fun)
		    (error 'unknown-macro-sub-character
			   :stream stream
			   :sub-char char2))
		  (return (funcall fun stream char2
				   (if parameter-given parameter nil))))))

(defun make-dispatch-macro-character
    (char &optional (non-terminating-p nil) (readtable *readtable*))
  (setf (gethash char (syntax-types readtable))
	(if non-terminating-p
	    :non-terminating-macro
	    :terminating-macro))
  (set-macro-character char 'sharpsign
		       non-terminating-p
		       readtable)
  (setf (gethash char (dispatch-macro-characters readtable))
	(make-hash-table))
  t)

(defun set-dispatch-macro-character
    (disp-char sub-char function &optional (readtable *readtable*))
  (when (digit-char-p sub-char)
    (error 'sub-char-must-not-be-a-decimal-digit
	   :disp-char disp-char
	   :sub-char sub-char))
  (setf sub-char (char-upcase sub-char))
  (let ((subtable (gethash disp-char (dispatch-macro-characters readtable))))
    (when (null subtable)
      (error 'char-must-be-a-dispatching-character
	     :disp-char disp-char))
    (setf (gethash sub-char subtable) function)))

(defun get-dispatch-macro-character
    (disp-char sub-char &optional (readtable *readtable*))
  ;; The HyperSpec does not say whether we should convert
  ;; to upper case here, but we think we should.
  (setf sub-char (char-upcase sub-char))
  (let ((subtable (gethash disp-char (dispatch-macro-characters readtable))))
    (when (null subtable)
      (error 'char-must-be-a-dispatching-character
	     :disp-char disp-char))
    (nth-value 0 (gethash sub-char subtable))))

(defun copy-readtable (&optional (from-readtable *readtable*) to-readtable)
  (when (null to-readtable)
    (setf to-readtable (make-instance 'readtable)))
  (clrhash (syntax-types to-readtable))
  (clrhash (macro-characters to-readtable))
  (maphash (lambda (key value)
	     (setf (gethash key (syntax-types to-readtable)) value))
	   (syntax-types from-readtable))
  (maphash (lambda (char entry)
	     (setf (gethash char (macro-characters to-readtable))
		   entry))
	   (macro-characters from-readtable))
  (maphash (lambda (char entry)
	     (let ((table (make-hash-table)))
	       (maphash (lambda (sub-char function)
			  (setf (gethash sub-char table) function))
			entry)
	       (setf (gethash char (dispatch-macro-characters to-readtable))
		     table)))
	   (dispatch-macro-characters from-readtable))
  (setf (readtable-case to-readtable)
	(readtable-case from-readtable))
  to-readtable)
