(cl:in-package #:sicl-sequence)

(defparameter *special-array-information*
  '(((simple-array character) . schar)
    ((array character) . char)
    ((simple-array (unsigned-byte 8)) . aref)
    ((array (unsigned-byte 8)) . aref)
    ((simple-array (signed-byte 8)) . aref)
    ((array (signed-byte 8)) . aref)
    ((simple-array (unsigned-byte 16)) . aref)))
