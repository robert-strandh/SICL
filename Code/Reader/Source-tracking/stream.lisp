(cl:in-package #:sicl-reader)

(defclass source-tracking-stream
    (trivial-gray-streams:fundamental-character-input-stream)
  ((%original :initarg :original :reader original)
   (%contents :initarg :contents :reader contents)
   (%index :initform 0 :accessor index)
   (%line :initform 0 :accessor line)
   (%column :initform 0 :accessor column)))
