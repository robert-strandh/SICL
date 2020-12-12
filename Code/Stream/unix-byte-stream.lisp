(cl:in-package #:sicl-stream)

(defgeneric file-descriptor (stream))

(defclass unix-byte-output-stream (stream buffered-output-stream-mixin)
  ((%file-descriptor :initarg :file-descriptor :reader file-descriptor)))

(defmethod stream-finish-output ((stream unix-byte-output-stream))
  (sicl-posix-high:write
   (file-descriptor stream)
   (buffer stream)
   :end (fill-pointer (buffer stream)))
  (setf (fill-pointer (buffer stream)) 0))

(defmethod stream-write-byte ((stream unix-byte-output-stream) integer)
  (when (= (fill-pointer (buffer stream))
	   (array-total-size (buffer stream)))
    (stream-finish-output stream))
  (vector-push integer (buffer stream)))
