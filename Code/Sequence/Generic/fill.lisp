(cl:in-package #:sicl-sequence)

(defmethod fill ((datum t) item &key &allow-other-keys)
  (error 'must-be-sequence
         :datum datum))

(defmethod fill ((list list) item &key (start 0) end)
  (for-each-relevant-cons (cons index list start end)
    (setf (car cons) item)))

(replicate-for-each-relevant-vectoroid #1=#:vectoriod
  (defmethod fill ((vectoroid #1#) item &key (start 0) end)
    (for-each-relevant-element (element index vectoroid start end)
      (setf element item))))
