(in-package #:sicl-stream)

(defclass character-to-binary-output-stream
    (fundamental-character-output-stream)
  ((%binary-stream :initarg :binary-stream :reader binary-stream)))

(defmethod stream-write-char
    ((stream character-to-binary-output-stream) character)
  (let ((code (char-code character))
        (output-stream (binary-stream stream))
        (m1 #b10000000)
        (m2 #b00111111))
    (cond ((< code #x80)
           (stream-write-byte output-stream code))
          ((< code #x800)
           (stream-write-byte
            output-stream
            (logior #b11000000 (ash code -6)))
           (stream-write-byte output-stream (logior m1 (logand code m2))))
          ((< code #x1000)
           (stream-write-byte output-stream (logior #b11100000 (ash code -12)))
           (stream-write-byte output-stream (logior m1 (logand (ash code -6) m2)))
           (stream-write-byte output-stream (logior m1 (logand code m2))))
          (t
           (stream-write-byte output-stream (logior #b11110000 (ash code -18)))
           (stream-write-byte output-stream (logior m1 (logand (ash code -12) m2)))
           (stream-write-byte output-stream (logior m1 (logand (ash code -6) m2)))
           (stream-write-byte output-stream (logior m1 (logand code m2)))))))

           
