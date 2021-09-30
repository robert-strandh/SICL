(cl:in-package #:sicl-random)

(defgeneric random-bits (random-state))

(defclass random-state ()
  ((%random-bits :initform (error "Amount of random bits must be specified.")
                 :reader random-bits
                 :documentation
                 "The amount of bits in the number returned by READ-RANDOM-STATE.")))

(defgeneric state-array (random-state))

(defgeneric index (random-state))

(defgeneric (setf index) (index random-state))

(defclass mt19937-random (random-state)
  ((%random-bits :initform +mt19937-bits+)
   (%state-array :initform (make-array +mt19937-size+
                                       :element-type '(unsigned-byte 32))
                 :initarg :state-array
                 :reader state-array)
   (%index :initform (1+ +mt19937-size+)
           :initarg :index
           :accessor index)))

(defgeneric state-num (random-state))

(defgeneric (setf state-num) (state-num random-state))

(defgeneric increment (random-state))

(defgeneric (setf increment) (increment random-state))

(defclass pcg32-random (random-state)
  ((%random-bits :initform 32)
   (%state-num :initarg :state-num
               :accessor state-num
               :type (unsigned-byte 64))
   (%increment :initarg :increment
               :accessor increment
               :type (unsigned-byte 64))))
