(cl:in-package #:sicl-allocator)

(defparameter *number-of-bins* 512)

;;; The address in memory where the vector of start sentinels starts.
(defparameter *start-sentinels-start* 0)

;;; The address in memory where the vector of end sentinels starts.
(defparameter *end-sentinels-start*
  (+ *start-sentinels-start* (* *number-of-bins* 8)))

;;; The address in memory where the vector of bin sizes starts.
(defparameter *bin-sizes-start*
  (+ *end-sentinels-start* (* *number-of-bins* 8)))
