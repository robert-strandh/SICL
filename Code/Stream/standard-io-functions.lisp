(cl:in-package #:sicl-stream)

(defun write-char
    (character &optional (output-stream *standard-output*))
  (stream-write-char output-stream character)
  character)

(defun terpri (&optional (output-stream *standard-output*))
  (write-char #\Newline output-stream))

(defun write-string
    (string
     &optional (output-stream *standard-output*)
     &key (start 0) (end nil))
  (stream-write-string output-stream string start end)
  string)

(defun write-line
    (string
     &optional (output-stream *standard-output*)
     &key (start 0) (end nil))
  (stream-write-string output-stream string start end)
  (terpri output-stream)
  string)

(defun write-byte (byte stream)
  (stream-write-byte stream byte)
  byte)

(defun finish-output (&optional (output-stream *standard-output*))
  (stream-finish-output output-stream)
  nil)

(defun force-output (&optional (output-stream *standard-output*))
  (stream-force-output output-stream)
  nil)

(defun clear-output (&optional (output-stream *standard-output*))
  (stream-clear-output output-stream)
  nil)

(defun read-char
    (&optional
       (input-stream *standard-input*)
       (eof-error-p t)
       (eof-value nil)
       (recursive-p nil))
  (let ((result (stream-read-char input-stream)))
    (declare (ignore recursive-p))
    (if (eq result :eof)
        (if eof-error-p
            (error 'end-of-file :error-stream input-stream)
            eof-value)
        result)))

(defun unread-char
    (character &optional (input-stream *standard-input*))
  (stream-unread-char input-stream character)
  nil)
