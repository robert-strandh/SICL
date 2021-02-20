(cl:in-package #:sicl-linear-probing-hash-table)

(deftype data-vector ()
  `(simple-array t 1))

(macrolet ((define-data-accessor (accessor-name offset)
             `(progn
                (declaim (inline ,accessor-name (setf ,accessor-name)))
                (defun ,accessor-name (vector n)
                  (declare (data-vector vector))
                  (aref vector (+ ,offset (* n 3))))
                (defun (setf ,accessor-name) (new-value vector n)
                  (setf (aref vector (+ ,offset (* n 3)))
                        new-value)))))
  (define-data-accessor hash 0)
  (define-data-accessor key 1)
  (define-data-accessor value 2))

(defconstant +empty+ '+empty+)
(defun make-data-vector (size)
  "Create a data vector for a hash table of a given size."
  (let ((data (make-array (* size 3) :initial-element +empty+)))
    (dotimes (n size)
      (setf (hash data n) 0))
    data))

(defun nearest-allowed-size (size)
  (max +metadata-entries-per-word+
       ;; This form returns the next power of 2 above SIZE.
       (expt 2 (integer-length (1- size)))))

(defmethod initialize-instance :after ((table linear-probing-hash-table) &key)
  (let ((size (nearest-allowed-size (hash-table-size table))))
    (setf (hash-table-data table) (make-data-vector size)
          (hash-table-metadata table) (make-metadata-vector size))))

(declaim (inline split-hash cheap-mod))
(defun split-hash (hash)
  (declare ((unsigned-byte 64) hash))
  (floor hash 256))

(defun cheap-mod (number divisor)
  "A cheap and usually incorrect MOD, which works when DIVISOR is a power of two."
  (logand number (1- divisor)))

(declaim (notinline call-with-key-index))
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
               (fixnum          size)
               (group-index probe-group))
      (loop
        (let ((probe-word (metadata-group metadata probe-group))
              (base-position (* probe-group +metadata-entries-per-word+)))
          (declare (fixnum base-position)
                   (metadata probe-word))
          (do-matches (offset (bytes (mask-h2 h2) probe-word))
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
                                (position (gensym) p-p))
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
       (call-with-key-index ,key ,hash-table ,metadata ,data ,size
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
         (setf (metadata metadata position) +tombstone-metadata+)
         (incf (hash-table-tombstone-count hash-table)))
        (t
         ;; Otherwise, we can keep this entry empty.
         (setf (metadata metadata position) +empty-metadata+)))
      (setf (key data position)   +empty+
            (value data position) +empty+)
      (decf (%hash-table-count hash-table))
      (return-from remhash t))
    nil))

(defun add-mapping (metadata data size test going-to-overwrite?
                    key new-value hash)
  (declare (metadata-vector metadata)
           (simple-vector   data)
           (function        test)
           (fixnum          size))
  (multiple-value-bind (h1 h2) (split-hash hash)
    (let* ((groups      (floor size +metadata-entries-per-word+))
           (probe-group (cheap-mod h1 groups)))
      (declare (fixnum groups probe-group))
      (loop
        (let ((probe-word (metadata-group metadata probe-group))
              (base-position (* probe-group +metadata-entries-per-word+)))
          (when going-to-overwrite?
            ;; First, try to find a binding to replace.
            (do-matches (offset (bytes (mask-h2 h2) probe-word))
              (let* ((position (+ offset base-position))
                     (stored-key (key data position)))
                (when (or (eq key stored-key)
                          (funcall test key stored-key))
                  (setf (value data position) new-value)
                  (return-from add-mapping (values t nil))))))
          ;; Then try to find an empty or tombstone value to write into.
          (do-matches (offset (writable probe-word))
            (let* ((position (+ offset base-position)))
              (setf (key   data position) key
                    (value data position) new-value
                    (hash  data position) hash
                    (metadata metadata position) (mask-h2 h2))
              (return-from add-mapping
                (values nil
                        (= +empty-metadata+
                           (group-metadata probe-word offset))))))
          (setf probe-group (cheap-mod (1+ probe-group) groups)))))))

(defmethod (setf gethash) (new-value key hash-table &optional default)
  (declare (ignore default))
  (let ((size (hash-table-size hash-table)))
    (multiple-value-bind (previously-key? previously-empty?)
        (add-mapping (hash-table-metadata hash-table)
                     (hash-table-data hash-table)
                     size
                     (%hash-table-test hash-table)
                     t
                     key new-value
                     (sicl-hash-table:hash hash-table key))
      (unless previously-key?
        (let ((count (incf (%hash-table-count hash-table))))
          (when previously-empty?
            (maybe-resize hash-table
                          count
                          (+ (hash-table-tombstone-count hash-table)
                             count)
                          size))))))
  new-value)

(defun maybe-resize (hash-table count count-with-tombstones size)
  ;; We resize (or compact) when size - 1 = count-with-tombstones, i.e.
  ;; there are no empty mappings now.
  (when (or (< (* size (hash-table-rehash-threshold hash-table)) count-with-tombstones)
            (= (1- size) count-with-tombstones))
    (let* (;; If the actual entry count is greater than REHASH-THRESHOLD, then
           ;; grow, else just copy to remove tombstones.
           (new-size (if (< (* size (hash-table-rehash-threshold hash-table))
                            count)
                         (* size 2)
                         size))
           (new-metadata (make-metadata-vector new-size))
           (new-data     (make-data-vector  new-size)))
      (copy-table-data (hash-table-metadata hash-table)
                       (hash-table-data hash-table)
                       size
                       new-metadata new-data
                       new-size)
      (setf (%hash-table-size hash-table)    new-size
            (hash-table-metadata hash-table) new-metadata
            (hash-table-data hash-table)     new-data))))

(defun copy-table-data (old-metadata old-data old-size new-metadata new-data new-size)
  (declare (metadata-vector old-metadata new-metadata)
           (data-vector old-data new-data))
  (dotimes (group-number (floor old-size +metadata-entries-per-word+))
    (declare (group-index group-number))
    (do-matches (offset (has-value (metadata-group old-metadata group-number)))
      (let* ((mapping-index (+ (* group-number +metadata-entries-per-word+)
                               offset))
             (hash  (hash  old-data mapping-index))
             (key   (key   old-data mapping-index))
             (value (value old-data mapping-index)))
        (declare (fixnum mapping-index))
        (add-mapping new-metadata new-data new-size
                     (lambda (k1 k2)
                       (declare (ignore k1 k2))
                       (error "The TEST should never be used when resizing."))
                     nil
                     key value hash)))))

(defmethod maphash (function (hash-table linear-probing-hash-table))
  (let ((data (hash-table-data hash-table)))
    (dotimes (n (hash-table-size hash-table))
      (let ((key   (key data n))
            (value (value data n)))
        (unless (or (eql key +empty+)
                    (eql value +empty+))
          (funcall function key value)))))
  nil)

(defmethod clrhash ((hash-table linear-probing-hash-table))
  (let ((size (hash-table-size hash-table)))
    (setf (hash-table-metadata hash-table) (make-metadata-vector size)
          (hash-table-data hash-table)     (make-data-vector size)
          (%hash-table-count hash-table) 0
          (hash-table-tombstone-count hash-table) 0))
  hash-table)
