(cl:in-package #:sicl-sequence)

(defmethod fill ((list list) item &key (start 0) end)
  (declare (method-properties inlineable))
  (for-each-relevant-cons (cons index list start end)
    (setf (car cons) item))
  list)

(seal-domain #'fill '(list t))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod fill ((vector #1#) item &key (start 0) end)
    (declare (method-properties inlineable))
    (for-each-relevant-element (element index vector start end)
      (setf element item))
    vector))

(seal-domain #'fill '(vector t))
