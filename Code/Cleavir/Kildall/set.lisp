(in-package #:cleavir-set)

;;;; Generic set interface,
;;;; plus list and bitset specializations, and utilities.
;;;; Not actually specific to kildall.

;;; using generics might slow things down enough to make bitsets
;;; pointless, at least on small code...

(defgeneric union (specialization set1 set2))
(defgeneric intersection (specialization set1 set2))
(defgeneric difference (specialization set1 set2))
(defgeneric exclusive-or (specialization set1 set2))

(defgeneric subsetp (specialization set1 set2))

;;; do not destroy sets
(defgeneric adjoin (specialization object set))
(defgeneric remove (specialization object set))
(defgeneric in-set-p (specialization object set))

(defgeneric make-set (specialization &rest objects))

;;; this function converts back to a CL set (i.e., list)
(defgeneric set->list (specialization set))

;;; CL list implementation

(defclass listset () ())

(defmethod union ((s listset) set1 set2)
  (cl:union set1 set2))
(defmethod intersection ((s listset) set1 set2)
  (cl:intersection set1 set2))
(defmethod difference ((s listset) set1 set2)
  (cl:set-difference set1 set2))
(defmethod exclusive-or ((s listset) set1 set2)
  (cl:set-exclusive-or set1 set2))

(defmethod subsetp ((s listset) set1 set2)
  (cl:subsetp set1 set2))

(defmethod adjoin ((s listset) object set)
  ;; could probably just push and skip the check, honestly
  (cl:adjoin object set))

(defmethod remove ((s listset) object set)
  (cl:remove object set))

(defmethod in-set-p ((s listset) object set)
  (find object set))

(defmethod make-set ((s listset) &rest objects)
  objects)

(defmethod set->list ((s listset) set)
  set)

;;; Bitset implementation
;;; Universes are hashes object -> position. There is no info on
;;; the reverse mapping, so that's slower - but that should be ok,
;;; since it's only used for set->list which is maybe slow anyway

(defclass bitset ()
  ((%universe :initarg :universe :reader universe)))

(declaim (inline universe-position))
(defun universe-position (specialization object)
  (gethash object (universe specialization)))

(declaim (inline universe-size))
;;; another slot?
(defun universe-size (specialization)
  (hash-table-count (universe specialization)))

(declaim (inline make-bitset))
(defun make-bitset (specialization)
  (make-array (universe-size specialization)
	      :element-type 'bit
	      :initial-element 0))

(defmethod union ((s bitset) set1 set2)
  (bit-ior set1 set2))
(defmethod intersection ((s bitset) set1 set2)
  (bit-and set1 set2))
(defmethod difference ((s bitset) set1 set2)
  (bit-andc2 set1 set2))
(defmethod exclusive-or ((s bitset) set1 set2)
  (bit-xor set1 set2))

(defmethod subsetp ((s bitset) set1 set2)
  (equal (bit-and set1 set2) set1))

(defmethod adjoin ((s bitset) object set)
  (let ((result (copy-seq set)))
    (setf (sbit result (universe-position s object)) 1)
    result))
(defmethod remove ((s bitset) object set)
  (let ((result (copy-seq set)))
    (setf (sbit result (universe-position s object)) 0)
    result))

(defmethod make-set ((s bitset) &rest objects)
  (let ((result (make-bitset s)))
    (loop for o in objects
	  do (setf (sbit result (universe-position s o)) 1))
    result))

(defmethod set->list ((s bitset) set)
  (loop for v being the hash-values of (universe s)
	  using (hash-key k)
	when (= (sbit set v) 1)
	  collect k))

;;; utilities: make universes from things.
;;; this part is dependent on cleavir-ir.
;;; the above could be a separate system/whatever with no deps.

;;; make a universe for instructions.
(defun instruction-universe (initial-instruction)
  ;; duplicates map-instructions a fair bit to save hashtables
  (let ((id 0) ; position in the vector
	(result (make-hash-table :test #'eq))
	(work-list (list initial-instruction)))
    (loop until (null work-list)
	  do (let ((work (pop work-list)))
	       (unless (nth-value 1 (gethash work result))
		 (setf (gethash work result) id)
		 (incf id))
	       (setf work-list
		     (append work-list (cleavir-ir:outputs work)))
	       (when (typep work 'cleavir-ir:enclose-instruction)
		 (push (cleavir-ir:code work) work-list))))
    result))

;;; predicate can be used to limit the data used (variables, eg)
(defun data-universe (initial-instruction predicate)
  ;; just use map-instructions, no hash tables to save
  (let ((id 0)
	(result (make-hash-table :test #'eq)))
    (flet ((add (datum)
	     (when (and (funcall predicate datum)
			(not (nth-value 1 (gethash datum result))))
	       (setf (gethash datum result) id)
	       (incf id))))
      (cleavir-ir:map-instructions-arbitrary-order
       (lambda (i)
	 (mapc #'add (cleavir-ir:inputs i))
	 (mapc #'add (cleavir-ir:outputs i)))
       initial-instruction))
    result))

;;; generic helper: make a universe from an arbitrary list of stuff
(defun universe-of-things (&rest things)
  (loop with result = (make-hash-table :test #'eq)
	for id from 0
	for thing in things
	do (setf (gethash thing result) id
		 id (1+ id))
	finally (return result)))
