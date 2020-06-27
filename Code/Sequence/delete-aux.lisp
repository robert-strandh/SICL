(cl:in-package #:sicl-sequence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; List Deletion

(declaim (inline delete-in-list))
(defun delete-in-list (predicate list from-end start end count)
  (let* ((count (canonicalize-count count))
         (head (cons nil list))
         (prev (nthcdr start head))
         (n-deleted 0))
    (declare (cons head prev))
    (declare (dynamic-extent head))
    (declare (fixnum count n-deleted))
    (when (plusp count)
      (if (not from-end)
          (loop for cons on (cdr prev)
                for index fixnum from start below (or end most-positive-fixnum)
                if (funcall predicate (car cons)) do
                  (setf (cdr prev) (cdr cons))
                  (incf n-deleted)
                  (when (= count n-deleted)
                    (return))
                else do
                  (setf prev cons))
          (with-list-start-and-end (start end) (list start end)
            (for-each-relevant-cons (prev index prev 0 (- end start) t)
              (let ((cons (cdr prev)))
                (when (funcall predicate (car cons))
                  (setf (cdr prev) (cdr cons))
                  (incf n-deleted)
                  (when (= count n-deleted)
                    (return))))))))
    (cdr head)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Vector Deletion

(declaim (inline delete-in-vector/from-start))
(defun delete-in-vector/from-start (predicate vector start end count)
  (let ((count (canonicalize-count count))
        (n-deleted 0))
    (declare (fixnum count n-deleted))
    (with-vector-start-and-end (start end length) (vector start end)
      (do ((index start (1+ index))
           (new-end start))
          ((or (= index end)
               (= n-deleted count))
           (if (zerop n-deleted)
               vector
               (do ((index index (1+ index))
                    (new-end new-end (1+ new-end)))
                   ((= index length)
                    (shrink-vector vector new-end))
                 (setf (elt vector new-end)
                       (elt vector index)))))
        (let ((element (elt vector index)))
          (cond ((funcall predicate element)
                 (incf n-deleted))
                (t
                 (setf (elt vector new-end) element)
                 (incf new-end))))))))

(declaim (inline delete-in-vector/from-end))
(defun delete-in-vector/from-end (predicate vector start end count)
  (with-vector-start-and-end (start end length) (vector start end)
    (let ((count (canonicalize-count count))
          (n-deleted 0))
      (declare (fixnum count n-deleted))
      (when (plusp count)
        (loop for index downfrom (1- end) to start
              for element = (elt vector index)
              if (funcall predicate element) do
                (incf n-deleted)
                (when (= n-deleted count)
                  (replace vector vector
                           :start1 index
                           :start2 (+ index n-deleted)
                           :end2 length)
                  (return))
              else do
                (setf (elt vector (+ index n-deleted)) element)
              finally
                 (replace vector vector
                          :start1 start
                          :start2 (+ start n-deleted)
                          :end2 length)))
      (shrink-vector vector (- length n-deleted)))))

(declaim (inline delete-in-vector))
(defun delete-in-vector (predicate vector from-end start end count)
  (if (not from-end)
      (delete-in-vector/from-start predicate vector start end count)
      (delete-in-vector/from-end predicate vector start end count)))
