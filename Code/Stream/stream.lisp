(cl:in-package #:sicl-stream)

(defclass stream () ())

(defmethod streamp (object)
  (declare (ignore object))
  nil)

(defmethod streamp ((object stream))
  (declare (ignorable object))
  t)

(defmethod input-stream-p ((stream stream))
  nil)

(defmethod output-stream-p ((stream stream))
  nil)

(defmethod interactive-stream-p ((stream stream))
  nil)

(defmethod stream-finish-output ((stream stream))
  nil)

(defmethod stream-force-output ((stream stream))
  nil)

(defmethod stream-clear-output ((stream stream))
  nil)

(defclass fundamental-stream
    (stream)
  ())

(defclass fundamental-input-stream
    (fundamental-stream)
  ())

(defmethod input-stream-p ((stream fundamental-input-stream))
  t)

(defclass fundamental-output-stream
    (fundamental-stream)
  ())

(defmethod output-stream-p ((stream fundamental-output-stream))
  t)

(defclass fundamental-character-stream
    (fundamental-stream)
  ())

(defmethod stream-element-type ((stream fundamental-character-stream))
  'character)

(defclass fundamental-binary-stream
    (fundamental-stream)
  ())

(defclass fundamental-character-input-stream
    (fundamental-input-stream fundamental-character-stream)
  ())

(defclass fundamental-character-output-stream
    (fundamental-output-stream fundamental-character-stream)
  ())

(defclass fundamental-binary-input-stream
    (fundamental-input-stream fundamental-binary-stream)
  ())

(defclass fundamental-binary-output-stream
    (fundamental-output-stream fundamental-binary-stream)
  ())

(defgeneric buffer (stream))

(defclass buffered-stream-mixin ()
  ((%buffer :initarg :buffer :reader buffer)))

(defclass buffered-output-stream-mixin (buffered-stream-mixin) ())

(defgeneric write-buffer (stream))

(defmethod stream-finish-output ((stream buffered-output-stream-mixin))
  (write-buffer (buffer stream))
  (stream-clear-output stream))

(defmethod force-output ((stream buffered-output-stream-mixin))
  (stream-finish-output stream))

(defmethod stream-clear-output ((stream buffered-output-stream-mixin))
  (setf (fill-pointer (buffer stream)) 0))

(defclass buffered-binary-output-stream-mixin
    (buffered-output-stream-mixin)
  ())

(defmethod stream-write-byte
    ((stream buffered-binary-output-stream-mixin) integer)
  (vector-push integer (buffer stream)))

(defmethod stream-write-byte :before
    ((stream buffered-binary-output-stream-mixin) integer)
  (declare (ignore integer))
  (let ((buffer (buffer stream)))
    (when (= (fill-pointer buffer) (array-total-size buffer))
      (stream-finish-output stream))))
