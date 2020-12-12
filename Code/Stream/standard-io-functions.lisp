(cl:in-package #:sicl-stream)

(defun write-char
    (character &optional (output-stream *standard-output*))
  (stream-write-char output-stream character)
  character)

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
