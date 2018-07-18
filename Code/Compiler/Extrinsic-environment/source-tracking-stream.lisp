(cl:in-package #:sicl-extrinsic-environment)

;;; Turn the contents of STREAM into a vector of lines, where each
;;; line is a string.
(defun read-entire-stream (stream)
  (let ((lines (loop for line = (read-line stream nil nil)
                     until (null line)
                     collect line)))
    (make-array (length lines) :initial-contents lines)))

(defclass source-tracking-stream ()
  (;; This slot contains a vector of lines where each line is a
   ;; string.
   (%lines :initarg :lines :reader lines)
   ;; This slot contains the index into LINES of the current line.
   (%current-line-index :initform 0 :accessor current-line-index)
   ;; This slot contains the index into a particular line of the
   ;; character about to be read.
   (%current-character-index :initform 0 :accessor current-character-index)))

(defun source-tracking-stream-from-stream (stream)
  (make-instance 'source-tracking-stream
    :lines (read-entire-stream stream)))
