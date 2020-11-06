(cl:in-package #:sicl-bucket-hash-table)

(defmethod initialize-instance :after ((hash-table bucket-hash-table) &key)
  (setf (hash-table-data hash-table)
        (make-array (hash-table-size hash-table)
                    :initial-element '()))
  (unless (slot-boundp hash-table 'hash-function)
    (setf (hash-table-hash-function hash-table)
          (find-hash-function (hash-table-test hash-table)))))

(defmethod make-hash-table-iterator ((hash-table bucket-hash-table))
  (let ((data (hash-table-data hash-table))
        (size (hash-table-size hash-table))
        (position 0)
        (contents '()))
    (lambda ()
      (block hash-table-iterator
        ;; Look for the next bucket which contains mappings.
        (loop while (null contents)
              when (= position size)
                ;; When we run out of mappings, we return the single value NIL.
                do (return-from hash-table-iterator nil)
              do (setf contents (aref data position))
                 (incf position))
        ;; Otherwise, we return values T, key and value.
        (let ((entry (pop contents)))
          (values t (car entry) (cdr entry)))))))

(defun grow-and-rehash (hash-table)
  (let* ((new-size (if (integerp (hash-table-rehash-size hash-table))
                       (+ (hash-table-size hash-table) (hash-table-rehash-size hash-table))
                       (round (* (hash-table-size hash-table) (hash-table-rehash-size hash-table)))))
         (new-data (make-array new-size :initial-element '())))
    (maphash (lambda (key value)
               (let* ((key-hash (funcall (hash-table-hash-function hash-table)
                                         (hash-table-offset hash-table)
                                         key))
                      (index (mod key-hash new-size)))
                 (push (cons key value) (aref new-data index))))
             hash-table)
    (setf (hash-table-data hash-table) new-data
          (%bucket-hash-table-size hash-table) new-size)))

(defun maybe-grow-and-rehash (hash-table)
  (when (> (hash-table-count hash-table)
           (* (hash-table-size hash-table) (hash-table-rehash-threshold hash-table)))
    (grow-and-rehash hash-table)))

(defmacro with-entry ((entries-index entries entry key hash-table) &body body)
  (let ((key-hash (gensym)))
    `(let* ((,key-hash (funcall (hash-table-hash-function ,hash-table)
                                (hash-table-offset ,hash-table)
                                ,key))
            (,entries-index (mod ,key-hash (hash-table-size ,hash-table)))
            (,entries (aref (hash-table-data ,hash-table) index))
            (,entry (assoc ,key entries :test (hash-table-test ,hash-table))))
       ,@body)))

(defmethod (setf gethash) (value key (hash-table bucket-hash-table) &optional default)
  (declare (ignore default))
  (with-entry (index entries entry key hash-table)
    (cond (entry
           (setf (cdr entry) value))
          (t
           (push (cons key value) (aref (hash-table-data hash-table) index))
           (incf (%bucket-hash-table-count hash-table))
           (maybe-grow-and-rehash hash-table))))
  value)

(defmethod gethash (key (hash-table bucket-hash-table) &optional default)
  (with-entry (index entries entry key hash-table)
    (if entry
        (values (cdr entry) t)
        (values nil default))))

(defmethod remhash (key (hash-table bucket-hash-table))
  (with-entry (index entries entry key hash-table)
    (unless (null entry)
      (setf (aref (hash-table-data hash-table) index)
            (delete key entries :key #'car :test (hash-table-test hash-table)))
      (decf (%bucket-hash-table-count hash-table)))
    (not (null entry))))

(defmethod clrhash ((hash-table bucket-hash-table))
  (fill (hash-table-data hash-table) '())
  (setf (%bucket-hash-table-count hash-table) 0)
  hash-table)

(defmethod maphash (function (hash-table bucket-hash-table))
  (loop for bucket across (hash-table-data hash-table)
        do (loop for (key . value) in bucket
                 do (funcall function key value))))

(setf sicl-hash-table:*default-hash-table-class* (find-class 'bucket-hash-table))
