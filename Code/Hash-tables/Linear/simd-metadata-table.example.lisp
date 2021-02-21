(cl:error "This file should not be loaded without modification.
This file is an example of how metadata-table.lisp can be modified to use
a single instruction-multiple data instruction set for probing.")

(cl:in-package #:sicl-linear-probing-hash-table)

(defconstant +metadata-entries-per-word+ 32
  "The number of metadata entries we store per group.")
(deftype metadata ()
  `(simd:byte-pack 32))
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

(declaim (inline bytes matches-p writable mask-h2))

(defun writable (word)
  "Return matches for metadata bytes we can put new mappings in."
  (simd:non-zero-bytes (simd:logand word (simd:broadcast +empty-metadata+))))

(defun has-value (word)
  "Return matches for metadata bytes that already have mappings."
  (simd:zero-bytes (simd:logand word (simd:broadcast +empty-metadata+))))

(defun mask-h2 (h2)
  "Mask off part of the H2 hash, for use as metadata."
  (declare ((unsigned-byte 8) h2))
  (logand #x7f h2))

(defun bytes (byte word)
  "Return matches for a byte in a metadata group."
  (declare ((unsigned-byte 8) byte))
  (simd:equal-bytes (simd:broadcast byte) word))

(defmacro do-matches ((position bit-mask) &body body)
  "Evaluate BODY with POSITION bound to every match in the provided BIT-MASK."
  (error "This body intentionally left blank.
A good start would be to use a 'count trailing zeroes' instruction to 
efficiently iterate over the matches in the bit mask."))

(defun matches-p (bit-mask)
  "Are there any matches in BIT-MASK?"
  (not (simd:is-zero bit-mask)))

(defun make-metadata-vector (size)
  "Create a metadata vector for a hash table of a given size, with all elements initialized to +EMPTY-METADATA+."
  (make-array size
              :element-type '(unsigned-byte 8)
              :initial-element +empty-metadata+))

(declaim (inline (setf %metadata) %metadata
                 metadata-group))

(defun (setf metadata) (new-byte vector position)
  "Set a metadata byte."
  (declare (metadata-vector vector)
           (vector-index position)
           ((unsigned-byte 8) new-byte))
  (setf (aref vector position) new-byte))

(defun metadata (vector position)
  "Retrieve a metadata byte."
  (declare (metadata-vector vector)
           (vector-index position))
  (aref vector position))

(defun metadata-group (vector position)
  "Retrieve the Nth metadata group from a vector. 
Note that N has a length of a group; on a 8-element-per-group implementation, 
(metadata-group V 1) retrieves the 8th through 15th metadata bytes of V."
  (declare (metadata-vector vector)
           (group-index n))
  (simd:load-pack vector n))
