(cl:in-package #:sicl-array)

(defmethod array-element-type (object)
  (error 'argument-to-array-element-type-must-be-an-array
         :datum object
         :expected-type 'array))

(defmethod array-element-type ((array array))
  (error 'argument-to-array-element-type-must-be-a-specialized-array
         :datum array
         :expected-type '(array *)))

(defmethod array-element-type ((array array-t))
  't)

(defmethod array-element-type ((array array-complex-double-float))
  '(complex double-float))

(defmethod array-element-type ((array array-complex-single-float))
  '(complex single-float))

(defmethod array-element-type ((array array-double-float))
  'double-float)

(defmethod array-element-type ((array array-single-float))
  'single-float)

(defmethod array-element-type ((array array-signed-byte-64))
  '(signed-byte 64))

(defmethod array-element-type ((array array-unsigned-byte-64))
  '(unsigned-byte 64))

(defmethod array-element-type ((array array-signed-byte-32))
  '(signed-byte 32))

(defmethod array-element-type ((array array-unsigned-byte-32))
  '(unsigned-byte 32))

(defmethod array-element-type ((array array-unsigned-byte-8))
  '(unsigned-byte 8))

(defmethod array-element-type ((array array-bit))
  'bit)

(defmethod array-element-type ((array array-character))
  'character)
