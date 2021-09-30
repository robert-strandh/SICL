(cl:in-package #:sicl-random)

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
