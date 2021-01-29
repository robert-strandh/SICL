(cl:in-package #:sicl-linear-probing-hash-table)

(macrolet ((define-storage-accessor (accessor-name offset)
             `(progn
                (declaim (inline ,accessor-name (setf ,accessor-name)))
                (defun ,accessor-name (vector n)
                  (aref vector (+ ,offset (* n 3))))
                (defun (setf ,accessor-name) (new-value vector n)
                  (setf (aref vector (+ ,offset (* n 3)))
                        new-value)))))
  (define-storage-accessor hash 0)
  (define-storage-accessor key 1)
  (define-storage-accessor value 2))

(defconstant +empty+ '+empty+)
(defun make-storage-vector (size)
  "Create a storage vector for a hash table of a given size."
  (let ((storage (make-array (* size 3) :initial-element +empty+)))
    (dotimes (n size)
      (setf (hash storage n) 0))
    storage))

(defun nearest-allowed-size (size)
  (max +metadata-entries-per-word+
       ;; This form returns the next power of 2 above SIZE.
       (expt 2 (integer-length (1- size)))))

(defmethod initialize-instance :after ((table linear-probing-hash-table) &key)
  (let ((size (nearest-allowed-size (hash-table-size table))))
    (setf (hash-table-data table) (make-storage-vector size)
          (hash-table-metadata table) (make-metadata-vector size))))

(declaim (inline split-hash cheap-mod))
(defun split-hash (hash)
  (floor hash 256))

(defun cheap-mod (number divisor)
  "A cheap and usually incorrect MOD, which works when DIVISOR is a power of two."
  (logand number (1- divisor)))

(declaim (inline call-with-key-index call-with-write-index)) 
(defun call-with-key-index (key hash-table metadata data size continuation)
  "Call CONTINUATION with each matching group, group position, offset in the group and actual position.
Compare this to WITH-ENTRY in the bucket hash table."
  (declare (optimize (speed 3))
           (function continuation))
  (multiple-value-bind (h1 h2)
      (split-hash (sicl-hash-table:hash hash-table key))
    (let* ((groups      (floor size +metadata-entries-per-word+))
           (probe-group (cheap-mod h1 groups))
           (test        (%hash-table-test hash-table)))
      (declare (metadata-vector metadata)
               (simple-vector   data)
               (function        test)
               (fixnum          size probe-group))
      (loop
        (let ((probe-word (metadata-group metadata probe-group))
              (base-position (* probe-group +metadata-entries-per-word+)))
          (do-matches (offset (bytes h2 probe-word))
            (let* ((position (+ offset base-position))
                   (stored-key (key data position)))
              (when (or (eq key stored-key)
                        (funcall test key stored-key))
                (return-from call-with-key-index
                  (funcall continuation probe-word base-position offset position)))))
          ;; If there are EMPTY values, then the mapping we want will not be
          ;; in the next group.
          (when (matches-p (bytes +empty-metadata+ probe-word))
            (return-from call-with-key-index))
          ;; Otherwise, try the next group.
          (setf probe-group (cheap-mod (1+ probe-group) groups)))))))

(defmacro with-key-index ((key hash-table metadata data size)
                          (&key (probe-word (gensym) p-w-p)
                                (group-position (gensym) g-p-p)
                                (offset (gensym) o-p)
                                (position (gensym) p-p)
                                (scanner 'call-with-key-index))
                          &body body)
  (let ((continuation (gensym "CONTINUATION")))
    `(flet ((,continuation (,probe-word ,group-position ,offset ,position)
              ,@(loop for supplied-p in (list p-w-p g-p-p o-p p-p)
                      for variable   in (list probe-word group-position offset position)
                      unless supplied-p
                        collect `(declare (ignore ,variable)))
              ,@body))
       (declare (inline ,continuation)
                (dynamic-extent #',continuation))
       (,scanner ,key ,hash-table ,metadata ,data ,size
                 #',continuation))))

(defmethod gethash (key (hash-table linear-probing-hash-table) &optional default)
  (let* ((metadata (hash-table-metadata hash-table))
         (data     (hash-table-data hash-table))
         (size     (hash-table-size hash-table)))
    (with-key-index (key hash-table metadata data size)
      (:position position)
      (return-from gethash (values (value data position) t)))
    (values default nil)))

(defmethod remhash (key (hash-table linear-probing-hash-table))
  (let* ((metadata (hash-table-metadata hash-table))
         (data     (hash-table-data hash-table))
         (size     (hash-table-size hash-table)))
    (with-key-index (key hash-table metadata data size)
      (:probe-word probe-word :position position)
      (cond
        ((not (matches-p (bytes +empty-metadata+ probe-word)))
         ;; If the group is full of entries and/or tombstones, then we have to
         ;; add another tombstone.
         (setf (metadata metadata position) +tombstone-metadata+))
        (t
         ;; Otherwise, we can keep this entry empty.
         (setf (metadata metadata position) +empty-metadata+)
         (decf (hash-table-count-with-tombstones hash-table))))
      (setf (key data position)   nil
            (value data position) nil)
      (decf (%hash-table-count hash-table))
      (return-from remhash t))
    nil))

(defmethod (setf gethash) (new-value key hash-table &optional default)
  (declare (ignore default))
  (let* ((metadata (hash-table-metadata hash-table))
         (data     (hash-table-data hash-table))
         (size     (hash-table-size hash-table))
         (hash     (sicl-hash-table:hash hash-table key)))
    (multiple-value-bind (h1 h2) (split-hash hash)
      (let* ((groups      (floor size +metadata-entries-per-word+))
             (probe-group (cheap-mod h1 groups))
             (test        (%hash-table-test hash-table)))
        (declare (metadata-vector metadata)
                 (simple-vector   data)
                 (function        test)
                 (fixnum          size probe-group))
        (loop
          (let ((probe-word (metadata-group metadata probe-group))
                (base-position (* probe-group +metadata-entries-per-word+)))
            ;; First, try to find a binding to replace.
            (do-matches (offset (bytes h2 probe-word))
              (let* ((position (+ offset base-position))
                     (stored-key (key data position)))
                (when (or (eq key stored-key)
                          (funcall test key stored-key))
                  (setf (value data position) new-value)
                  (return-from gethash new-value))))
            ;; Then try to find an empty or tombstone value to write into.
            (do-matches (offset (writable probe-word))
              (let* ((position (+ offset base-position)))
                (setf (key   data position) key
                      (value data position) new-value
                      (hash  data position) hash
                      (metadata metadata position) h2)
                (incf (%hash-table-count hash-table))
                (when (= +empty-metadata+
                         (group-metadata probe-word offset))
                  (incf (hash-table-count-with-tombstones hash-table)))
                (return-from gethash new-value)))
            ;; Otherwise, try the next group.
            (setf probe-group (cheap-mod (1+ probe-group) groups))))))))
