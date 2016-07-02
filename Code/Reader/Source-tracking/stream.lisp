(cl:in-package #:sicl-reader)

(defun make-contents ()
  (make-array 10
	      :element-type 'character
	      :adjustable t))

(defclass source-tracking-stream
    (trivial-gray-streams:fundamental-character-input-stream)
  ((%original :initarg :original :reader original)
   (%contents :initform (make-contents) :reader contents)
   (%index :initform 0 :accessor index)
   (%line :initform 0 :accessor line)
   (%column :initform 0 :accessor column)))
