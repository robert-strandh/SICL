(cl:in-package #:sicl-sequence)

(defgeneric copy-seq (sequence))

(defgeneric elt (sequence index))

(defgeneric (setf elt) (value sequence index))

(defgeneric fill (sequence item &key start end))

(defgeneric make-sequence-like (sequence length &key initial-element initial-contents))

(defgeneric adjust-sequence (sequence length &key initial-element initial-contents))

(defgeneric subseq (sequence start &optional end))

(defgeneric (setf subseq) (value sequence start &optional end))

(defgeneric map-into (result-sequence function &rest sequences))

(defgeneric reduce (function sequence &key key from-end start end initial-value))

(defgeneric count (item sequence &key from-end start end key test test-not))

(defgeneric count-if (predicate sequence &key from-end start end key))

(defgeneric count-if-not (predicate sequence &key from-end start end key))

(defgeneric length (sequence))

(defgeneric reverse (sequence))

(defgeneric nreverse (sequence))

(defgeneric sort (sequence predicate &key key))

(defgeneric stable-sort (sequence predicate &key key))

(defgeneric find (item sequence &key from-end test test-not start end key))

(defgeneric find-if (predicate sequence &key from-end start end key))

(defgeneric find-if-not (predicate sequence &key from-end start end key))

(defgeneric position (item sequence &key from-end test test-not start end key))

(defgeneric position-if (predicate sequence &key from-end start end key))

(defgeneric position-if-not (predicate sequence &key from-end start end key))

(defgeneric search (sequence-1 sequence-2 &key from-end test test-not key start1 start2 end1 end2))

(defgeneric mismatch (sequence-1 sequence-2 &key from-end test test-not key start1 start2 end1 end2))

(defgeneric replace (sequence-1 sequence-2 &key start1 end1 start2 end2))

(defgeneric substitute (newitem olditem sequence &key from-end test test-not start end count key))

(defgeneric substitute-if (newitem predicate sequence &key from-end start end count key))

(defgeneric substitute-if-not (newitem predicate sequence &key from-end start end count key))

(defgeneric nsubstitute (newitem olditem sequence &key from-end test test-not start end count key))

(defgeneric nsubstitute-if (newitem predicate sequence &key from-end start end count key))

(defgeneric nsubstitute-if-not (newitem predicate sequence &key from-end start end count key))

(defgeneric remove (item sequence &key from-end test test-not start end count key))

(defgeneric remove-if (test sequence &key from-end start end count key))

(defgeneric remove-if-not (test sequence &key from-end start end count key))

(defgeneric delete (item sequence &key from-end test test-not start end count key))

(defgeneric delete-if (test sequence &key from-end start end count key))

(defgeneric delete-if-not (test sequence &key from-end start end count key))

(defgeneric remove-duplicates (sequence &key from-end test test-not start end key))

(defgeneric delete-duplicates (sequence &key from-end test test-not start end key))
