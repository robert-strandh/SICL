(in-package #:sicl-array)

(deftype simple-array (&optional element-type dimension-spec)
  `(array ,element-type ,dimension-spec))

(deftype simple-string (&optional size)
  `(string ,size))

(deftype base-string (&optional size)
  `(string ,size))

(deftype simple-base-string (&optional size)
  `(string ,size))

(deftype simple-bit-vector (&optional size)
  `(bit-vector ,size))
