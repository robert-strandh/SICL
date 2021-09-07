(in-package #:sicl-global-environment)

(deftype vector (&optional element-type size)
  `(array ,element-type (,size)))

(deftype simple-vector (&optional size)
  `(simple-array t (,size)))

(deftype string (&optional size)
  `(array character (,size)))

(deftype simple-string (&optional size)
  `(simple-array character (,size)))

(deftype base-string (&optional size)
  `(array base-char (,size)))

(deftype simple-base-string (&optional size)
  `(simple-array base-char (,size)))

(deftype bit-vector (&optional size)
  `(array bit (,size)))

(deftype simple-bit-vector (&optional size)
  `(simple-array bit (,size)))
