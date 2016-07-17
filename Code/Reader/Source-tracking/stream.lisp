(cl:in-package #:sicl-source-tracking-reader)

(defun make-contents ()
  (make-array 10
	      :fill-pointer 0
	      :element-type 'character
	      :adjustable t))

(defclass source-tracking-stream
    (trivial-gray-streams:fundamental-character-input-stream)
  ((%original :initarg :original :reader original)
   (%contents :initform (make-contents) :reader contents)
   (%index :initform 0 :accessor index)
   (%line :initform 0 :accessor line)
   (%column :initform 0 :accessor column)
   (%char-to-read :initform nil :accessor char-to-read)))

(defun make-source-tracking-stream (stream)
  (make-instance 'source-tracking-stream
    :original stream))

(defmethod trivial-gray-streams:stream-read-char
    ((stream source-tracking-stream))
  (if (null (char-to-read stream))
      (let ((char (read-char (original stream))))
	(vector-push-extend char (contents stream) 10)
	(if (eql char #\Newline)
	    (progn (setf (column stream) 0)
		   (incf (line stream)))
	    (incf (column stream)))
	char)
      (prog1 (char-to-read stream)
	(setf (char-to-read stream) nil))))

(defmethod trivial-gray-streams:stream-unread-char
    ((stream source-tracking-stream) character)
  (setf (char-to-read stream) character))
