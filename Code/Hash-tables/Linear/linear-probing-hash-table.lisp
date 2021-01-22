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
       (integer-length (1- size))))

(defmethod initialize-instance :after ((table linear-probing-hash-table) &key)
  (let ((size (nearest-allowed-size (hash-table-size table))))
    (setf (hash-table-data table) (make-storage-vector size)
          (hash-table-metadata table) (make-metadata-vector size))))

(declaim (inline split-hash cheap-mod))
(defun split-hash (hash-table key)
  (let ((hash (sicl-hash-table:hash hash-table key)))
    (floor hash 256)))

(defun cheap-mod (number divisor)
  "A cheap and usually incorrect MOD, which works when DIVISOR is a power of two."
  (logand number (1- divisor)))

(defmethod gethash (key (hash-table linear-probing-hash-table) &optional default)
  (multiple-value-bind (h1 h2) (split-hash hash-table key)
    (let* ((metadata (hash-table-metadata hash-table))
           (data     (hash-table-data hash-table))
           (size     (hash-table-size hash-table))
           (probe-position (cheap-mod h1 size))
           (test     (%hash-table-test hash-table)))
      (declare (metadata-vector metadata)
               (simple-vector   data))
      (let ((probe-word (aref metadata probe-position)))
        (do-matches (offset (bytes h2 probe-word))
          (let* ((position (+ offset
                              (* probe-position +metadata-entries-per-word+)))
                 (stored-key (key data position)))
            (when (funcall test key stored-key)
              (let ((value (value data position)))
                (unless (eq value +empty+)
                  (return-from gethash (values value t)))))))
        (unless (zerop (bytes +empty-metadata+ probe-word))
          (return-from gethash (values default nil)))))))
        
