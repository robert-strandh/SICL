(cl:in-package #:sicl-stream)

(defgeneric streamp (object))

(defgeneric input-stream-p (stream))

(defgeneric output-stream-p (stream))

(defgeneric interactive-stream-p (stream))

(defgeneric stream-element-type (stream))

(defgeneric close (stream))
