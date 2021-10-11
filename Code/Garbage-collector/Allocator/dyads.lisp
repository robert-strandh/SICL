(cl:in-package #:sicl-allocator)

(defparameter *dyads-start* (ash 1 20))

(defparameter *dyads-count* (ash 1 25))

(defparameter *dyads-end* (+ *dyads-start* (* 16 *dyads-count*)))

(defparameter *free-list* 0)

(defparameter *next-dyad* *dyads-start*)

(defun allocate-dyad ()
  (if (zerop *free-list*)
      (if (= *next-dyad* *dyads-end*)
          (error 'storage-condition)
          (prog1 *next-dyad* (incf *next-dyad* 16)))
      (prog1 *free-list*
        (setf *free-list*
              (sicl-memory:memory-unsigned *free-list* 64)))))

(defun initialize-dyads ()
  (setf *next-dyad* *dyads-start*)
  (setf *free-list* 0))
