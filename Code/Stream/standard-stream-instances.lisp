(cl:in-package #:sicl-stream)

(defparameter *unix-standard-output*
  (make-instance 'unix-byte-output-stream
    :file-descriptor 1
    :buffer
    (make-instance 'sicl-array:vector-unsigned-byte-8
      :dimensions '(256)
      :fill-pointer 0
      :additional-space 32)))

(defparameter *standard-output*
  (make-instance 'character-to-binary-output-stream
    :binary-stream *unix-standard-output*))
