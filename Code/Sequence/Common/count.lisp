(cl:in-package :sicl-sequence)

(defun count-list (item list from-end test test-not start end key)
  (declare (optimize (speed 3) (debug 0) (safety 3)))
  (declare (type list list))
  (with-bounding-indices-list (start end)
    (with-key-function key
      (with-test-and-test-not (test test-not)
        (with-from-end from-end
          (let ((result 0))
            (declare (type fixnum result))
            (for-each-relevant-cons (cons index list start end from-end)
              (let ((element (apply-key-function (car cons) key)))
                (when (satisfies-two-argument-test-p item element test test-not)
                  (incf result))))
            (return-from count-list result)))))))

;;; A version of COUNT, specialized to a vector.  ITEM is the item to
;;; count.  VECTOR is the vector to search.  FROM-END is a generalized
;;; Boolean indicating the direction of the search.  At least one of
;;; TEST and TEST-NOT must be NIL.  If both are NIL it is as if TEST
;;; had the value #'EQL.  If one of TEST and TEST-NOT is not NIL, it
;;; must be a designator for a function.  START and END must be
;;; non-negative fixnums.  KEY must be a designator for function.
(defun count-vector (item vector from-end test test-not start end key)
  (declare (optimize (speed 3) (debug 0) (safety 3)))
  (declare (type fixnum start end))
  (with-key-function key
    (with-test-and-test-not (test test-not)
      (with-from-end from-end
        (with-vector-type vector
          (let ((result 0))
            (declare (type fixnum result))
            (for-each-relevant-element (e index vector start end from-end)
              (let ((element (apply-key-function e key)))
                (when (satisfies-two-argument-test-p item element test test-not)
                  (incf result))))
            (return-from count-vector result)))))))

(defun count-aux (item sequence from-end test test-not start end key)
  ;; (declare (optimize (debug 0) (speed 3) (safety 0))
  ;;       (type (and fixnum (integer 0)) start)
  ;;       (type (or null (and fixnum (integer 0))) end))
  (unless (and (integerp start) (>= start 0))
    (error 'invalid-start-index-type
           :expected-type '(integer 0)
           :datum start))
  (unless (or (null end) (and (integerp end) (>= end 0)))
    (error 'invalid-end-index-type
           :expected-type '(or null (integer 0))
           :datum end))
  (canonicalize-test-and-test-not test test-not count)
  (canonicalize-key key)
  (if (listp sequence)
      (count-list item sequence from-end test test-not start end key)
      (progn (when (null end)
               (setf end (length sequence)))
             (count-vector item sequence from-end test test-not start end key))))

(defun count-if-list (predicate list from-end start end key)
  (declare (optimize (speed 3) (debug 0) (safety 3)))
  (declare (type function predicate))
  (with-key-function key
    (with-from-end from-end
      (let ((result 0))
        (declare (type fixnum result))
        (for-each-relevant-cons (cons index list start end from-end)
          (let ((element (apply-key-function (car cons) key)))
            (when (funcall predicate element)
              (incf result))))
        (return-from count-if-list result)))))

(defun count-if-vector (predicate vector from-end start end key)
  (declare (optimize (speed 3) (debug 0) (safety 3)))
  (declare (type fixnum start end))
  (declare (type function predicate))
  (with-key-function key
    (with-from-end from-end
      (with-vector-type vector
        (let ((result 0))
          (declare (type fixnum result))
          (for-each-relevant-element (e index vector start end from-end)
            (let ((element (apply-key-function e key)))
              (when (funcall predicate element)
                (incf result))))
          (return-from count-if-vector result))))))

(defun count-if-not-list (predicate list from-end start end key)
  (declare (optimize (speed 3) (debug 0) (safety 3)))
  (declare (type function predicate))
  (with-key-function key
    (with-from-end from-end
      (let ((result 0))
        (declare (type fixnum result))
        (for-each-relevant-cons (cons index list start end from-end)
          (let ((element (apply-key-function (car cons) key)))
            (unless (funcall predicate element)
              (incf result))))
        (return-from count-if-not-list result)))))

(defun count-if-not-vector (predicate vector from-end start end key)
  (declare (optimize (speed 3) (debug 0) (safety 3)))
  (declare (type fixnum start end))
  (declare (type function predicate))
  (with-key-function key
    (with-from-end from-end
      (with-vector-type vector
        (let ((result 0))
          (declare (type fixnum result))
          (for-each-relevant-element (e index vector start end from-end)
            (let ((element (apply-key-function e key)))
              (unless (funcall predicate element)
                (incf result))))
          (return-from count-if-not-vector result))))))
