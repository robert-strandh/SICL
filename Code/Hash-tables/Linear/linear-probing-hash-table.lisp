(cl:in-package #:sicl-linear-probing-hash-table)

(defconstant +empty+ '+empty+)

(macrolet ((define-storage-accessor (accessor-name offset)
             `(progn
                (declaim (inline ,accessor-name (setf ,accessor-name)))
                (defun ,accessor-name (vector n)
                  (aref vector (+ ,offset (* n 3))))
                (defun (setf ,accessor-name) (new-value vector n)
                  (setf (aref vector (+ ,offset (* n 3)))
                        new-value)))))
  (define-storage-accessor hash 0)
  (define-storage-accessor key 1)
  (define-storage-accessor value 2))

(defun make-storage-vector (size)
  "Create a storage vector for a hash table of a given size."
  (let ((storage (make-array (* size 3) :initial-element +empty+)))
    (dotimes (n size)
      (setf (hash storage n) 0))
    storage))

(defmethod initialize-instance :after ((table linear-probing-hash-table) &key)
  (let ((size (nearest-multiple-of-metadata-size (hash-table-size table))))
    (setf (hash-table-data table) (make-storage-vector size)
          (hash-table-metadata table) (make-metadata-vector size))))
