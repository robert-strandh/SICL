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
