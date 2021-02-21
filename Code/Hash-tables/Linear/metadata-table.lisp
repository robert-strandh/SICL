(cl:in-package #:sicl-linear-probing-hash-table)

(defconstant +metadata-entries-per-word+ 8
  "The number of metadata entries we store per word. We read and write
entire words of metadata (each unit of metadata is one byte) to exploit
bit-level parallelism while scanning the metadata table.")
(deftype metadata ()
  `(unsigned-byte ,(* +metadata-entries-per-word+ 8)))
(deftype metadata-vector ()
  `(simple-array metadata 1))
(deftype group-index ()
  `(integer 0 ,(floor array-total-size-limit +metadata-entries-per-word+)))
(deftype vector-index ()
  `(and fixnum unsigned-byte))

(defconstant +empty-metadata+     #x80
  "The metadata byte stored for an empty entry.")
(defconstant +tombstone-metadata+ #x81
  "The metadata byte stored for a tombstoned entry.")

(defconstant +high-bits+ #x8080808080808080)
(defconstant +low-bits+  #x0101010101010101)

(declaim (inline zeroes bytes matches-p writable mask-h2))
(defun zeroes (word)
  (logand (lognot word)
          (- word +low-bits+)
          +high-bits+))

(defun writable (word)
  "Return matches for metadata bytes we can put new mappings in."
  (logand (* +empty-metadata+ +low-bits+) word))

(defun has-value (word)
  "Return matches for metadata bytes that already have mappings."
  (logxor +high-bits+ (logand +high-bits+ word)))

(defun mask-h2 (h2)
  "Mask off part of the H2 hash, for use as metadata."
  (declare ((unsigned-byte 8) h2))
  (logand #x7f h2))

(defun bytes (byte word)
  "Return matches for a byte in a metadata group."
  (declare ((unsigned-byte 8) byte))
  (zeroes (logxor word (* byte +low-bits+))))

(defmacro do-matches ((position bit-mask) &body body)
  "Evaluate BODY with POSITION bound to every match in the provided BIT-MASK."
  (let ((bit-mask-gensym (gensym "BIT-MASK")))
    `(let ((,bit-mask-gensym ,bit-mask))
       (unless (zerop ,bit-mask-gensym)
         (loop with matches of-type (unsigned-byte 64) = ,bit-mask
               for ,position below 8
               when (logtest #x80 matches)
                 do (progn ,@body)
               do (setf matches (ash matches -8)))))))

(defun matches-p (bit-mask)
  "Are there any matches in BIT-MASK?"
  (plusp bit-mask))

(defun make-metadata-vector (size)
  "Create a metadata vector for a hash table of a given size, with all elements initialized to +EMPTY-METADATA+."
  (make-array (floor size +metadata-entries-per-word+)
              :element-type 'metadata
              :initial-element (* +low-bits+ +empty-metadata+)))

(declaim (inline (setf metadata) (setf %metadata)
                 metadata %metadata
                 metadata-group))

(defun (setf %metadata) (new-byte vector word-position byte-position)
  (declare (metadata-vector vector)
           ((unsigned-byte 8) new-byte))
  (setf (ldb (byte 8 (* 8 byte-position)) (aref vector word-position))
        new-byte))

(defun %metadata (vector word-position byte-position)
  (declare (metadata-vector vector))
  (ldb (byte 8 (* 8 byte-position)) (aref vector word-position)))

(defun (setf metadata) (new-byte vector position)
  "Set a metadata byte."
  (declare (metadata-vector vector)
           (vector-index position)
           ((unsigned-byte 8) new-byte))
  (multiple-value-bind (word-position byte-position)
      (floor position +metadata-entries-per-word+)
    (setf (%metadata vector word-position byte-position) new-byte)))

(defun metadata (vector position)
  "Retrieve a metadata byte."
  (declare (metadata-vector vector)
           (vector-index position))
  (multiple-value-bind (word-position byte-position)
      (floor position +metadata-entries-per-word+)
    (%metadata vector word-position byte-position)))

(defun metadata-group (vector n)
  "Retrieve the Nth metadata group from a vector. 
Note that N has a length of a group; on a 8-element-per-group implementation, 
(metadata-group V 1) retrieves the 8th through 15th metadata bytes of V."
  (declare (metadata-vector vector)
           (group-index n))
  (the metadata (aref vector n)))
