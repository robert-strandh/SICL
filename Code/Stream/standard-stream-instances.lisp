(cl:in-package #:sicl-stream)

(setf *unix-standard-output*
      (make-instance 'unix-byte-output-stream
        :file-descriptor 1
        :buffer (make-instance 'sicl-array:vector-unsigned-byte-8
                  :dimensions '(256)
                  :fill-pointer 0
                  :additional-space 32)))

(setf *standard-output*
      (make-instance 'character-to-binary-output-stream
        :binary-stream *unix-standard-output*))

(setf *trace-output* *standard-output*)

(setf *unix-error-output*
      (make-instance 'unix-byte-output-stream
        :file-descriptor 2
        :buffer (make-instance 'sicl-array:vector-unsigned-byte-8
                  :dimensions '(256)
                  :fill-pointer 0
                  :additional-space 32)))

(setf *error-output*
      (make-instance 'character-to-binary-output-stream
        :binary-stream *unix-error-output*))

(setf *unix-standard-input*
      (make-instance 'unix-byte-input-stream :file-descriptor 0))

(setf *standard-input*
      (make-instance 'binary-to-character-input-stream
        :binary-stream *unix-standard-input*))

(setf *terminal-io*
      (make-two-way-stream *standard-input* *standard-output*))

(setf *query-io*
      (make-synonym-stream '*terminal-io*))

(setf *debug-io* *query-io*)
