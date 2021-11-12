(cl:in-package #:sicl-stream)

(defparameter *unix-standard-output*
  (make-instance 'unix-byte-output-stream :file-descriptor 1
                 :buffer (make-instance 'sicl-array:vector-unsigned-byte-8
                                        :dimensions '(256)
                                        :fill-pointer 0
                                        :additional-space 32)))

(defparameter *standard-output*
  (make-instance 'character-to-binary-output-stream
    :binary-stream *unix-standard-output*))

(defparameter *trace-output* *standard-output*)

(defparameter *unix-error-output*
  (make-instance 'unix-byte-output-stream :file-descriptor 2
                 :buffer (make-instance 'sicl-array:vector-unsigned-byte-8
                                        :dimensions '(256)
                                        :fill-pointer 0
                                        :additional-space 32)))

(defparameter *error-output*
  (make-instance 'character-to-binary-output-stream
    :binary-stream *unix-error-output*))

(defparameter *unix-standard-input*
  (make-instance 'unix-byte-input-stream :file-descriptor 0))

(defparameter *standard-input*
  (make-instance 'binary-to-character-input-stream
    :binary-stream *unix-standard-input*))

(defparameter *terminal-io*
  (make-two-way-stream *standard-input* *standard-output*))

(defparameter *query-io*
  (make-synonym-stream '*terminal-io*))

(defparameter *debug-io* *query-io*)
