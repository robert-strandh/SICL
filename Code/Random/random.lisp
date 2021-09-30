(cl:in-package #:sicl-random)

(defparameter *default-random-state-class-name* 'mt19937-random)
(defvar *random-state*)

(defun random-state-p (object)
  (typep object 'random-state))

(defun random-float (limit random-state)
  (declare (type float limit))
  (let ((bits (random-bits random-state)))
    (+ (mod (read-random-state random-state) (truncate limit))
       (* (float (mod (read-random-state random-state) (expt 2 bits)))
          (/ (typecase limit
               (single-float 1.0)
               (double-float 1.d0))
             (expt 2 bits))))))

(defun stitch-together-random-numbers (limit random-state)
  (let ((bits (random-bits random-state)))
    (let ((how-many (ceiling (integer-length limit) bits))
          (result 0))
      (dotimes (i how-many)
        (setf result
              (dpb (read-random-state random-state)
                   (byte bits (* bits i)) result)))
      (mod result limit))))

(defun make-random-state (&optional state)
  (declare (type (or random-state boolean) state))
  (flet ((initial-seed () ; TODO: Better way to get an initial seed.
           (coerce (get-universal-time) 'fixnum)))
    (cond ((random-state-p state)
           (copy-random-state state))
          ((eq state t)
           (let ((new-state (make-instance *default-random-state-class-name*)))
             (seed-random-state new-state (initial-seed))
             new-state))
          ((null state)
           (copy-random-state *random-state*)))))

(setf *random-state* (make-random-state t)) ; Sets the initial *random-state*

(defun random (limit &optional (random-state *random-state*))
  "Returns a non-negative pseudo-random number that is less than LIMIT."
  (declare (type (or integer real) limit)
           (type random-state random-state)
           (inline random-float stitch-together-numbers))
  (cond ((and (typep limit 'float) (> limit 0))
         (random-float limit random-state))
        ((and (typep limit 'integer) (> (integer-length limit) (random-bits random-state)))
         (stitch-together-random-numbers limit random-state))
        ((and (typep limit 'integer) (> limit 0))
         (mod (read-random-state random-state) limit))
        (t
         (error 'simple-type-error
                :expected-type '(or (integer 1) (float (0)))
                :datum limit
                :format-control "~S is neither a positive integer nor a positive float."
                :format-arguments (list limit)))))
