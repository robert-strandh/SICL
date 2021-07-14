(cl:in-package #:sicl-stream)

(defgeneric stream-string (stream))

(defclass string-stream (fundamental-character-input-stream)
  ((%string :initarg :string :reader stream-string)))

(defgeneric index (stream))

(defclass string-input-stream (string-stream)
  ((%index :initform 0 :accessor index)))

(defmethod stream-read-char ((stream string-input-stream))
  (prog1 (aref (stream-string stream) (index stream))
    (incf (index stream))))

(defmethod stream-unread-char
    ((stream string-input-stream) (character character))
  (decf (index stream))
  (assert (eql (aref (stream-string stream) (index stream))
               character)))

(defclass string-output-stream (string-stream)
  ())
