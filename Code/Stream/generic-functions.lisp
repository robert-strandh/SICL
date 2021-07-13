(cl:in-package #:sicl-stream)

(defgeneric streamp (object))

(defgeneric input-stream-p (stream))

(defgeneric output-stream-p (stream))

(defgeneric interactive-stream-p (stream))

(defgeneric stream-element-type (stream))

(defgeneric close (stream))

(defgeneric stream-finish-output (stream))

(defgeneric stream-force-output (stream))

(defgeneric stream-clear-output (stream))

(defgeneric stream-write-byte (stream integer))

(defgeneric stream-write-char (stream character))

(defgeneric stream-write-string (stream string &optional start end))

(defgeneric stream-read-byte (stream))

(defgeneric stream-read-char (stream))

(defgeneric stream-unread-char (stream character))

