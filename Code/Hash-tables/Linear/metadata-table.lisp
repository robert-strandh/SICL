(cl:in-package #:sicl-linear-probing-hash-table)

(defconstant +metadata-entries-per-word+ 8
  "The number of metadata entries we store per word. We read and write
entire words of metadata (each unit of metadata is one byte) to exploit
bit-level parallelism while scanning the metadata table.")

(defconstant +empty-metadata+     #x80
  "The metadata byte stored for an empty entry.")
(defconstant +tombstone-metadata+ #x81
  "The metadata byte stored for a tombstoned entry.")

(defun nearest-multiple-of-metadata-size (size)
  "Round off a hash table size to the nearest correct size."
  (* +metadata-entries-per-word+
     (ceiling size +metadata-entries-per-word+)))

(defconstant +high-bits+ #x8080808080808080)
(defconstant +low-bits+  #x0101010101010101)

(declaim (inline zeroes bytes))
(defun zeroes (word)
  (logand (lognot word)
          (- word +low-bits+)
          +high-bits+))

(defun bytes (byte word)
  (zeroes (logxor word (* byte +low-bits+))))

(defmacro do-matches ((byte word position) &body body)
  `(loop with matches of-type (unsigned-byte 64) = (bytes ,byte ,word)
         for ,position below 8
         when (logtest #x80 matches)
           do (progn ,@body)
         do (setf matches (ash matches -8))))

(defun make-metadata-vector (size)
  "Create a metadata vector for a hash table of a given size."
  (make-array (floor size +metadata-entries-per-word+)
              :element-type '(unsigned-byte 64)
              :initial-element (* +low-bits+ +empty-metadata+)))
