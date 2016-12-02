(cl:in-package #:sicl-stream)

(defclass unix-byte-stream ()
  ((%file-descriptor :initarg :file-descriptor :reader file-descriptor)
   (%buffer :initform (make-array 256
				  :element-type '(unsigned-byte 8)
				  :fill-pointer 0)
	    :initarg :buffer
	    :accessor buffer)))

(defclass sbcl-unix-byte-stream (unix-byte-stream)
  ((%fd-stream :initarg :fd-stream :reader fd-stream)))

(defmethod initialize-instance :after ((instance sbcl-unix-byte-stream) &key)
  (reinitialize-instance
   instance
   :fd-stream (sb-sys:make-fd-stream (file-descriptor instance)
				     :output t
				     :element-type '(unsigned-byte 8))))

(defmethod finish-output ((stream sbcl-unix-byte-stream))
  (write-sequence (buffer stream) (fd-stream stream)
		  :end (fill-pointer (buffer stream)))
  (cl:finish-output (fd-stream stream))
  (setf (fill-pointer (buffer stream)) 0))

(defmethod write-byte (integer (stream unix-byte-stream))
  (when (= (fill-pointer (buffer stream))
	   (array-total-size (buffer stream)))
    (finish-output stream))
  (vector-push integer (buffer stream)))

