(cl:in-package #:sicl-allocator-test)

(defun total-space ()
  (loop with allocated-space = 0
        with free-space = 0
        for chunk = sicl-allocator::*heap-start*
          then (+ chunk (sicl-allocator::chunk-size chunk))
        until (= chunk sicl-allocator::*heap-end*)
        do (if (sicl-allocator::chunk-free-p chunk)
               (incf free-space (sicl-allocator::chunk-size chunk))
               (incf allocated-space (sicl-allocator::chunk-size chunk)))
        finally (return (values allocated-space free-space))))

(defparameter *chunks* (make-array 100000))

(defparameter *next-chunk-position* 0)

(defparameter *allocate-p* t)

(defparameter *operations* '())

(defun one-iteration ()
  (when (< (random 1d0) 0.01)
    (setf *allocate-p* (not *allocate-p*)))
  (multiple-value-bind (allocated-space-before free-space-before)
      (total-space)
    (if (or (zerop *next-chunk-position*)
            (zerop allocated-space-before)
            (and *allocate-p*
                 (> free-space-before 10000)
                 (< *next-chunk-position* (length *chunks*))))
        (let* ((size (* 8 (+ 4 (random 1000))))
               (ignore (push `(allocate ,size) *operations*))
               (chunk (sicl-allocator::allocate-chunk size))
               (chunk-size (sicl-allocator::chunk-size chunk)))
          (declare (ignore ignore))
          (setf (cddr (car *operations*)) (list chunk))
          (assert (or (< (- chunk-size size) (* 4 8))
                      (<= size chunk-size (* size 1.2))))
          (assert (not (sicl-allocator::chunk-free-p chunk)))
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
          (push `(free ,victim-chunk) *operations*)
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
  (setf *next-chunk-position* 0)
  (setf *allocate-p* t)
  (setf *operations* '())
  (sicl-allocator::init)
  (loop repeat n
        do (one-iteration)))

(defun execute-operation (operation)
  (destructuring-bind (name argument) operation
    (if (eq name 'allocate)
        (sicl-allocator::allocate-chunk argument)
        (sicl-allocator::free argument))))

(defun execute-operations (operations)
  (sicl-allocator::init)
  (loop for operation in operations
        do (execute-operation operation)))

(defun invoke-with-chunks-in-bin (bin-offset function)
  (let ((start-sentinel-address
          (+ sicl-allocator::*start-sentinels-start* bin-offset))
        (end-sentinel-address
          (+ sicl-allocator::*end-sentinels-start* bin-offset)))
    (loop for chunkprev = (sicl-memory:memory start-sentinel-address 64)
            then (sicl-memory:memory (+ chunkprev 8) 64)
          until (= chunkprev end-sentinel-address)
          do (funcall function (- chunkprev 8)))))
