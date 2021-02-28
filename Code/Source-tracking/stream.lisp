(cl:in-package #:sicl-source-tracking)

;;; Turn the contents of STREAM into a vector of lines, where each
;;; line is a string.
(defun read-entire-stream (stream)
  (let ((lines (loop for line = (read-line stream nil nil)
                     until (null line)
                     collect line)))
    (make-array (length lines) :initial-contents lines)))

(defclass source-tracking-stream (stream)
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

;;; Gray stream programming
(defmethod trivial-gray-streams:stream-peek-char
    ((stream source-tracking-stream))
  (let ((char (trivial-gray-streams:stream-read-char stream)))
    (unless (eq char :eof)
      (trivial-gray-streams:stream-unread-char stream char))
    char))

(defmethod trivial-gray-streams:stream-read-char
    ((stream source-tracking-stream))
  (with-accessors ((lines lines)
                   (current-line-index current-line-index)
                   (current-character-index current-character-index ))
      stream
    (if (= current-line-index (length lines))
        :eof
        (let ((current-line (aref lines current-line-index)))
          (if (= current-character-index (length current-line))
              (prog1 #\Newline
                (incf current-line-index)
                (setf current-character-index 0))
              (prog1 (aref current-line current-character-index)
                (incf current-character-index)))))))

(defmethod trivial-gray-streams:stream-unread-char
    ((stream source-tracking-stream) character)
  (declare (ignore character))
  (with-accessors ((lines lines)
                   (current-line-index current-line-index)
                   (current-character-index current-character-index ))
      stream
    (if (zerop current-character-index)
        (progn (decf current-line-index)
               (setf current-character-index
                     (length (aref lines current-line-index))))
        (decf current-character-index))))

;;; FIXME: Use SICL-specific client.
(defmethod eclector.parse-result:source-position
    (client (stream source-tracking-stream))
  (make-instance 'source-position
    :lines (lines stream)
    :line-index (current-line-index stream)
    :character-index (current-character-index stream)))

;;; FIXME: Use SICL-specific client.
(defmethod eclector.parse-result:make-source-range (client start end)
  (if (and (typep start 'source-position)
           (typep end 'source-position))
      (cons start end)
      nil))

(defmacro with-source-tracking-stream-from-file
    ((stream-var file-spec) &body body)
  `(with-open-file (,stream-var ,file-spec :direction :input)
     (let ((,stream-var (source-tracking-stream-from-stream ,stream-var)))
       ,@body)))
