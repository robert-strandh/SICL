(cl:in-package #:sicl-hash-table)

(defstruct (hash-table (:constructor %make-hash-table)
                       (:print-function print-hash-table))
  test
  size
  data
  count
  rehash-size
  rehash-threshold)

(defun print-hash-table (hash-table stream depth)
  (declare (ignore depth))
  (print-unreadable-object (hash-table stream :type t :identity t)
    (format stream ":TEST ~A :COUNT ~A"
            (hash-table-test hash-table)
            (hash-table-count hash-table))))

(defun make-hash-table (&key (test #'eql) (size 16) (rehash-size 2.0) (rehash-threshold 1.0))
  (let ((hash-table (%make-hash-table :test test :size size
                                      :rehash-size rehash-size
                                      :rehash-threshold rehash-threshold)))
    (setf (hash-table-data hash-table) (make-array size :initial-element nil)
          (hash-table-count hash-table) 0)
    hash-table))

(defmacro with-hash-table-iterator ((name hash-table) &body body)
  (let ((function-name (make-symbol (concatenate 'string (symbol-name name) "-FUNCTION"))))
    `(let ((index -1)
           (entries-rest nil))
       (labels ((,function-name ()
                (unless (endp entries-rest)
                  (setf entries-rest (rest entries-rest)))
                (loop while (endp entries-rest)
                      do (incf index)
                         (when (= index (hash-table-size ,hash-table))
                           (return-from ,function-name nil))
                         (setf entries-rest (aref (hash-table-data ,hash-table) index)))
                (let ((entry (first entries-rest)))
                  (values t (car entry) (cdr entry)))))
         (macrolet ((,name ()
                      '(funcall #',function-name)))
           ,@body)))))

(defun maphash (function hash-table)
  (with-hash-table-iterator (next-entry hash-table)
    (loop (multiple-value-bind (more key value) (next-entry)
            (unless more (return nil))
            (funcall function key value)))))

(defun grow-and-rehash (hash-table)
  (let* ((new-size (if (integerp (hash-table-rehash-size hash-table))
                       (+ (hash-table-size hash-table) (hash-table-rehash-size hash-table))
                       (round (* (hash-table-size hash-table) (hash-table-rehash-size hash-table)))))
         (new-data (make-array new-size :initial-element nil)))
    (maphash (lambda (key value)
               (let* ((key-hash (sxhash key))
                      (index (mod key-hash new-size)))
                 (push (cons key value) (aref new-data index))))
             hash-table)
    (setf (hash-table-data hash-table) new-data
          (hash-table-size hash-table) new-size)))

(defun maybe-grow-and-rehash (hash-table)
  (when (> (hash-table-count hash-table)
           (* (hash-table-size hash-table) (hash-table-rehash-threshold hash-table)))
    (grow-and-rehash hash-table)))

(defmacro with-entry ((entries-index entries entry key hash-table) &body body)
  (let ((key-hash (gensym)))
    `(let* ((,key-hash (sxhash ,key))
            (,entries-index (mod ,key-hash (hash-table-size ,hash-table)))
            (,entries (aref (hash-table-data ,hash-table) index))
            (,entry (assoc ,key entries :test (hash-table-test ,hash-table))))
       ,@body)))

(defun puthash (key hash-table value)
  (with-entry (index entries entry key hash-table)
    (cond (entry
           (setf (cdr entry) value))
          (t
           (push (cons key value) (aref (hash-table-data hash-table) index))
           (incf (hash-table-count hash-table))
           (maybe-grow-and-rehash hash-table))))
  hash-table)

(defun gethash (key hash-table)
  (with-entry (index entries entry key hash-table)
    (if entry
        (values (cdr entry) t)
        (values nil nil))))

(defsetf gethash puthash)

(defun remhash (key hash-table)
  (with-entry (index entries entry key hash-table)
    (unless (null entry)
      (setf (aref (hash-table-data hash-table) index)
            (delete key entries :key #'car :test (hash-table-test hash-table)))
      (decf (hash-table-count hash-table)))
    (not (null entry))))

(defun clrhash (hash-table)
  (loop for index below (hash-table-size hash-table)
        do (setf (aref (hash-table-data hash-table) index) nil))
  (setf (hash-table-count hash-table) 0)
  hash-table)
