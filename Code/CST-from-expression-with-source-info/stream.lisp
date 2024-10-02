(cl:in-package #:sicl-cst-from-expression-with-source-info)

(defclass vector-of-lines-stream (gs:fundamental-character-output-stream)
  ((%contents
    :initform (make-array 0 :adjustable t :fill-pointer 0)
    :reader contents)
   (%current-line-index
    :initform 0
    :accessor current-line-index)
   (%current-character-index
    :initform 0
    :accessor current-character-index)))

(defmethod gs:stream-write-char ((stream vector-of-lines-stream) character)
  (if (eql character #\Newline)
      (progn
        (when (zerop (current-character-index stream))
          (vector-push-extend "" (contents stream)))
        (incf (current-line-index stream))
        (setf (current-character-index stream) 0))
      (progn 
        (when (zerop (current-character-index stream))
          (vector-push-extend
           (make-array 0
                       :element-type 'character
                       :adjustable t
                       :fill-pointer 0)
           (contents stream)))
        (vector-push-extend
         character
         (aref (contents stream) (current-line-index stream)))
        (incf (current-character-index stream)))))

(defmethod gs:stream-advance-to-column
    ((stream vector-of-lines-stream) column)
  (loop until (= (current-character-index stream) column)
        do (write-char #\Space stream)))
