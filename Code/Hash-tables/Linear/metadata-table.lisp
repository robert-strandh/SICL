(cl:in-package #:sicl-linear-probing-hash-table)

(defconstant +metadata-entries-per-word+ 8
  "The number of metadata entries we store per word. We read and write
entire words of metadata (each unit of metadata is one byte) to exploit
bit-level parallelism while scanning the metadata table.")
(deftype metadata ()
  `(unsigned-byte ,(* +metadata-entries-per-word+ 8)))
(deftype metadata-vector ()
  `(simple-array metadata 1))

(defconstant +empty-metadata+     #x80
  "The metadata byte stored for an empty entry.")
(defconstant +tombstone-metadata+ #x81
  "The metadata byte stored for a tombstoned entry.")

(defconstant +high-bits+ #x8080808080808080)
(defconstant +low-bits+  #x0101010101010101)

(declaim (inline zeroes bytes matches-p))
(defun zeroes (word)
  (logand (lognot word)
          (- word +low-bits+)
          +high-bits+))

(defun bytes (byte word)
  (zeroes (logxor word (* byte +low-bits+))))

(defun writable (word)
  (logand (* +empty-metadata+ +low-bits+) word))

(defmacro do-matches ((position bit-mask) &body body)
  (let ((bit-mask-gensym (gensym "BIT-MASK")))
    `(let ((,bit-mask-gensym ,bit-mask))
       (unless (zerop ,bit-mask-gensym)
         (loop with matches of-type (unsigned-byte 64) = ,bit-mask
               for ,position below 8
               when (logtest #x80 matches)
                 do (progn ,@body)
               do (setf matches (ash matches -8)))))))

(defun matches-p (bit-mask)
  (plusp bit-mask))

(defun make-metadata-vector (size)
  "Create a metadata vector for a hash table of a given size, with all elements initialized to +EMPTY-METADATA+."
  (make-array (floor size +metadata-entries-per-word+)
              :element-type 'metadata
              :initial-element (* +low-bits+ +empty-metadata+)))

(declaim (inline (setf metadata) (setf %metadata)
                 metadata %metadata
                 metadata-group group-metadata))

(defun (setf %metadata) (new-byte vector word-position byte-position)
  (declare (metadata-vector vector)
           ((unsigned-byte 8) new-byte))
  (setf (ldb (byte 8 (* 8 byte-position)) (aref vector word-position))
        new-byte))

(defun %metadata (vector word-position byte-position)
  (declare (metadata-vector vector))
  (ldb (byte 8 (* 8 byte-position)) (aref vector word-position)))

(defun (setf metadata) (new-byte vector position)
  (declare (metadata-vector vector)
           (vector-index position)
           ((unsigned-byte 8) new-byte))
  (multiple-value-bind (word-position byte-position)
      (floor position +metadata-entries-per-word+)
    (setf (%metadata vector word-position byte-position) new-byte)))

(defun metadata (vector position)
  (declare (metadata-vector vector)
           (vector-index position))
  (multiple-value-bind (word-position byte-position)
      (floor position +metadata-entries-per-word+)
    (%metadata vector word-position byte-position)))

(defun metadata-group (vector position)
  (declare (metadata-vector vector)
           (vector-index position))
  (the metadata (aref vector position)))

(defun group-metadata (word position)
  (declare (metadata word)
           ((mod 8) position))
  (ldb (byte 8 (* 8 position)) word))
