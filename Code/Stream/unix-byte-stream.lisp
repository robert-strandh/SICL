(cl:in-package #:sicl-stream)

(defgeneric file-descriptor (stream))

(defclass unix-byte-output-stream (cyclosis:binary-output-stream)
  ((%file-descriptor :initarg :file-descriptor :reader file-descriptor)))

(defmethod cyclosis:stream-finish-output ((stream unix-byte-output-stream))
  (sicl-posix-high:write (file-descriptor stream)
                         (cyclosis:binary-output-stream-buffer stream)
                         :end (fill-pointer (cyclosis:binary-output-stream-buffer stream)))
  (setf (fill-pointer (cyclosis:binary-output-stream-buffer stream)) 0))

(defclass unix-byte-input-stream (cyclosis:fundamental-binary-input-stream)
  ((%file-descriptor :initarg :file-descriptor :reader file-descriptor)))
