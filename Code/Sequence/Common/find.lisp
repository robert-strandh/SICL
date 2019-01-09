(cl:in-package :sicl-sequence)

#+sbcl(declaim (sb-ext:muffle-conditions sb-ext:code-deletion-note))

(defun find-list (item list from-end test test-not start end key)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type list list))
  (with-bounding-indices-list (start end)
    (with-key-function key
      (with-test-and-test-not (test test-not)
        (with-from-end from-end
          (for-each-relevant-cons (cons index list start end from-end)
            (let ((element (apply-key-function (car cons) key)))
              (when (satisfies-two-argument-test-p item element test test-not)
                (return-from find-list element)))))))))

;;; A version of FIND, specialized to a vector.  ITEM is the item to
;;; find.  VECTOR is the vector to search.  FROM-END is a generalized
;;; Boolean indicating the direction of the search.  At least one of
;;; TEST and TEST-NOT must be NIL.  If both are NIL it is as if TEST
;;; had the value #'EQL.  If one of TEST and TEST-NOT is not NIL, it
;;; must be a designator for a function.  START and END must be
;;; non-negative fixnums.  KEY must be a designator for function.
(defun find-vector (item vector from-end test test-not start end key)
  (declare (optimize (speed 3) (debug 0) (safety 3)))
  (declare (type fixnum start end))
  (with-key-function key
    (with-test-and-test-not (test test-not)
      (with-from-end from-end
        (with-vector-type vector
          (for-each-relevant-element (e index vector start end from-end)
            (let ((element (apply-key-function e key)))
              (when (satisfies-two-argument-test-p item element test test-not)
                (return-from find-vector element)))))))))

(defun find-aux (item sequence from-end test test-not start end key)
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
  (canonicalize-test-and-test-not test test-not find)
  (canonicalize-key key)
  (if (listp sequence)
      (find-list item sequence from-end test test-not start end key)
      (progn (when (null end)
               (setf end (length sequence)))
             (find-vector item sequence from-end test test-not start end key))))

(defun find-if-list (predicate list from-end start end key)
  (declare (optimize (speed 3) (debug 0) (safety 3)))
  (declare (type function predicate))
  (with-key-function key
    (with-from-end from-end
      (for-each-relevant-cons (cons index list start end from-end)
        (let ((element (apply-key-function (car cons) key)))
          (when (funcall predicate element)
            (return-from find-if-list element)))))))

(defun find-if-vector (predicate vector from-end start end key)
  (declare (optimize (speed 3) (debug 0) (safety 3)))
  (declare (type fixnum start end))
  (declare (type function predicate))
  (with-key-function key
    (with-from-end from-end
      (with-vector-type vector
        (for-each-relevant-element (e index vector start end from-end)
          (let ((element (apply-key-function e key)))
            (when (funcall predicate element)
              (return-from find-if-vector element))))))))

(defun find-if-not-list (predicate list from-end start end key)
  (declare (optimize (speed 3) (debug 0) (safety 3)))
  (declare (type function predicate))
  (with-key-function key
    (with-from-end from-end
      (for-each-relevant-cons (cons index list start end from-end)
        (let ((element (apply-key-function (car cons) key)))
          (unless (funcall predicate element)
            (return-from find-if-not-list element)))))))

(defun find-if-not-vector (predicate vector from-end start end key)
  (declare (optimize (speed 3) (debug 0) (safety 3)))
  (declare (type fixnum start end))
  (declare (type function predicate))
  (with-key-function key
    (with-from-end from-end
      (with-vector-type vector
        (for-each-relevant-element (e index vector start end from-end)
          (let ((element (apply-key-function e key)))
            (unless (funcall predicate element)
              (return-from find-if-not-vector element))))))))
