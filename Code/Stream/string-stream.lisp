(cl:in-package #:sicl-stream)

(defgeneric stream-string (stream))

(defclass string-stream (fundamental-character-input-stream)
  ((%string :initarg :string :reader stream-string)))

(defgeneric start (stream))

(defgeneric end (stream))

(defgeneric index (stream))

(defclass string-input-stream (string-stream)
  ((%start :initarg :start :accessor start)
   (%end :initarg :end :accessor end)
   (%index :initform 0 :accessor index)))

(defmethod stream-read-char ((stream string-input-stream))
  (if (>= (index stream) (end stream))
      :eof
      (prog1 (aref (stream-string stream) (index stream))
        (incf (index stream)))))

(defmethod stream-unread-char
    ((stream string-input-stream) (character character))
  (assert (> (index stream) (start stream)))
  (decf (index stream))
  (assert (eql (aref (stream-string stream) (index stream))
               character)))

(defclass string-output-stream (string-stream)
  ())
