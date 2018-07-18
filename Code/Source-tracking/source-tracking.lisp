(cl:in-package #:sicl-source-tracking)

;;; Our source location contains the vector of lines of the entire
;;; file, a line index, and a character index within the line.
(defclass source-position ()
  ((%lines :initarg :lines :reader lines)
   (%line-index :initarg :line-index :reader line-index)
   (%character-index :initarg :character-index :reader character-index)))

;;; FIXME: Use SICL-specific client.
(defmethod eclector.concrete-syntax-tree:source-position
    ((stream source-tracking-stream) client)
  (make-instance 'source-position
    :lines (lines stream)
    :line-index (current-line-index stream)
    :character-index (current-character-index stream)))
