(cl:in-package #:sicl-allocator-test)

(defun total-space ()
  (loop with allocated-space = 0
        with free-space = 0
        for chunk = sicl-allocator::*heap-start*
          then (+ chunk (sicl-allocator::chunk-size chunk))
        until (= chunk (sicl-gc-memory:end-memory))
        do (if (sicl-allocator::chunk-free-p chunk)
               (incf free-space (sicl-allocator::chunk-size chunk))
               (incf allocated-space (sicl-allocator::chunk-size chunk)))
        finally (return (values allocated-space free-space))))

(defparameter *chunks* (make-array 100000))

(defparameter *next-chunk-position* 0)

(defparameter *allocate-p* t)

(defun one-iteration ()
  (when (< (random 1d0) 0.001)
    (setf *allocate-p* (not *allocate-p*)))
  (multiple-value-bind (allocated-space-before free-space-before)
      (total-space)
    (if (or (zerop *next-chunk-position*)
            (zerop allocated-space-before)
            (and *allocate-p*
                 (> free-space-before 10000)
                 (< *next-chunk-position* (length *chunks*))))
        (let* ((size (* 8 (+ 4 (random 1000))))
               (chunk (sicl-allocator::allocate-chunk size))
               (chunk-size (sicl-allocator::chunk-size chunk)))
          (assert (<= size chunk-size (* size 1.1)))
          (setf (aref *chunks* *next-chunk-position*) chunk)
          (incf *next-chunk-position*)
          (multiple-value-bind (allocated-space-after free-space-after)
              (total-space)
            (assert (= allocated-space-after
                       (+ allocated-space-before chunk-size)))
            (assert (= free-space-after
                       (- free-space-before chunk-size)))))
        (let* ((victim-index (random *next-chunk-position*))
               (victim-chunk (aref *chunks* victim-index))
               (chunk-size (sicl-allocator::chunk-size victim-chunk)))
          (decf *next-chunk-position*)
          (rotatef (aref *chunks* victim-index)
                   (aref *chunks* *next-chunk-position*))
          (sicl-allocator::free victim-chunk)
          (multiple-value-bind (allocated-space-after free-space-after)
              (total-space)
            (assert (= allocated-space-after
                       (- allocated-space-before chunk-size)))
            (assert (= free-space-after
                       (+ free-space-before chunk-size))))))))

(defun test (n)
  (sicl-allocator::init)
  (loop repeat n
        do (one-iteration)))
