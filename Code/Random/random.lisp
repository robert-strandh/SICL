(cl:in-package #:sicl-random)

(defclass random-state ()
  ((%random-bits :initform (error "Amount of random bits must be specified.")
                 :reader random-bits
                 :documentation
                 "The amount of bits in the number returned by READ-RANDOM-STATE.")))

(defgeneric seed-random-state (state seed)
  (:documentation "Initializes and sets the initial seed of STATE."))
(defgeneric read-random-state (state)
  (:documentation "Returns the next random number from STATE and prepares the next one."))
(defgeneric copy-random-state (state)
  (:documentation "Returns a new object which is a copy of STATE."))

(defun random-state-p (object)
  (typep object 'random-state))

(defparameter *default-random-state-class-name* 'mt19937-random)
(defvar *random-state*)

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
    (let ((how-many (ceiling (/ (log limit 2) bits)))
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

;;; Mersenne Twister (MT19937)

(defconstant +mt19937-bits+ 32)
(defconstant +mt19937-size+ 624)
(defconstant +mt19937-period+ 397)
(defconstant +mt19937-a+ #x9908B0DF)
(defconstant +mt19937-u+ 29)
(defconstant +mt19937-s+ 7)
(defconstant +mt19937-b+ #x9D2C5680)
(defconstant +mt19937-t+ 15)
(defconstant +mt19937-c+ #xEFC60000)
(defconstant +mt19937-l+ 18)
(defconstant +mt19937-lower-mask+ #x7FFFFFFF)
(defconstant +mt19937-upper-mask+ #x80000000)

(defclass mt19937-random (random-state)
  ((%random-bits :initform +mt19937-bits+)
   (%state-array :initform (make-array +mt19937-size+
                                       :element-type '(unsigned-byte 32))
                 :initarg :state-array
                 :reader state-array)
   (%index :initform (1+ +mt19937-size+)
           :initarg :index
           :accessor index)))

(defmethod seed-random-state ((state mt19937-random) (seed fixnum))
  (setf (aref (state-array state) 0) seed)
  (loop :for i :from 1 :below +mt19937-size+
     :for xi-1 := (aref (state-array state) (1- i))
     :do
       (setf (aref (state-array state) i)
             (logand #xFFFFFFFF
                     (+ i (* 1812433253 (logxor xi-1 (ash xi-1 -30)))))))
  (setf (index state) +mt19937-size+))

(defmethod read-random-state ((state mt19937-random))
  (flet ((twist (state)
           (loop :for i :from 0 :below +mt19937-size+ :do
                (let* ((x (+ (logand (aref (state-array state) i)
                                     +mt19937-upper-mask+)
                             (logand (aref (state-array state)
                                           (mod (1+ i) +mt19937-size+))
                                     +mt19937-lower-mask+)))
                       (xa (ash x -1)))
                  (when (= 1 (ldb (byte 1 0) x))
                    (setf xa (logxor xa +mt19937-a+)))
                  (setf (aref (state-array state) i)
                        (logxor
                         (aref (state-array state)
                               (mod (+ i +mt19937-period+) +mt19937-size+))
                         xa))))
           (setf (index state) 0)))
    (when (> (index state) +mt19937-size+)
      (error "Never seeded."))
    (when (= (index state) +mt19937-size+)
      (twist state))
    (let* ((y (aref (state-array state) (index state)))
           (y (logxor y (ash y (- +mt19937-u+))))
           (y (logxor y (logand (ash y +mt19937-s+) +mt19937-b+)))
           (y (logxor y (logand (ash y +mt19937-t+) +mt19937-c+)))
           (y (logxor y (ash y -1))))
      (incf (index state))
      (ldb (byte +mt19937-bits+ 0) y))))

(defmethod copy-random-state ((state mt19937-random))
  (make-instance 'mt19937-random
                 :index (index state)
                 :state-array (copy-seq (state-array state))))

(defmethod print-object ((object mt19937-random) stream)
  (format stream "#.(make-instance 'sicl-random::mt19937-random :index ~D :state-array ~S)"
          (index object) (state-array object)))

;;; Sets the initial *random-state*

(setf *random-state* (make-random-state t))

(defun random (limit &optional (random-state *random-state*))
  "Returns a non-negative pseudo-random number that is less than LIMIT."
  (declare (type (or integer real) limit)
           (type random-state random-state)
           (inline random-float stitch-together-numbers))
  (cond ((and (typep limit 'float) (> limit 0))
         (random-float limit random-state))
        ((and (typep limit 'integer) (> (log limit 2) (random-bits random-state)))
         (stitch-together-random-numbers limit random-state))
        ((and (typep limit 'integer) (> limit 0))
         (mod (read-random-state random-state) limit))
        (t
         (error 'simple-type-error
                :expected-type '(or (integer 1) (float (0)))
                :datum limit
                :format-control "~S is neither a positive integer nor a positive float."
                :format-arguments (list limit)))))
