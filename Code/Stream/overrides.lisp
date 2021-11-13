(in-package #:sicl-stream)

(defmethod cyclosis:stream-peek-char-skip-whitespace
    ((stream cyclosis:fundamental-character-input-stream))
  (eclector.reader:peek-char t stream nil nil))


