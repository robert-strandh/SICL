(cl:in-package #:sicl-array)

(defgeneric default-element (array))

(defmethod default-element ((array array-t))
  nil)

(defmethod default-element ((array array-character))
  #\Space)

(defmethod default-element ((array array-bit))
  0)

(defmethod default-element ((array array-unsigned-byte-8))
  0)

(defmethod default-element ((array array-signed-byte-32))
  0)

(defmethod default-element ((array array-unsigned-byte-32))
  0)

(defmethod default-element ((array array-signed-byte-64))
  0)

(defmethod default-element ((array array-unsigned-byte-64))
  0)

(defmethod default-element ((array array-single-float))
  0f0)

(defmethod default-element ((array array-double-float))
  0d0)

(defmethod default-element ((array array-complex-single-float))
  #c(0f0 0f0))

(defmethod default-element ((array array-complex-double-float))
  #c(0d0 0d0))
