(cl:in-package #:sicl-stream)

(defgeneric file-descriptor (stream))

(defgeneric buffer (stream))

(defclass unix-byte-output-stream ()
  ((%file-descriptor :initarg :file-descriptor :reader file-descriptor)
   (%buffer :initform (make-array 256
				  :element-type '(unsigned-byte 8)
				  :fill-pointer 0)
	    :initarg :buffer
	    :accessor buffer)))

(defmethod finish-output ((stream unix-byte-output-stream))
  (sicl-posix-high:write
   (file-descriptor stream)
   (buffer stream)
   :end (fill-pointer (buffer stream)))
  (setf (fill-pointer (buffer stream)) 0))

(defmethod write-byte (integer (stream unix-byte-output-stream))
  (when (= (fill-pointer (buffer stream))
	   (array-total-size (buffer stream)))
    (finish-output stream))
  (vector-push integer (buffer stream)))
