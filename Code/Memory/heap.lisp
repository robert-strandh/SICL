(cl:in-package #:sicl-memory)

(defparameter *heap-start* (expt 2 32))

(defparameter *heap-size* (expt 2 32))

(defparameter *heap-end* (+ *heap-start* *heap-size*))

(defparameter *free-1* *heap-start*)

(defparameter *free-2* *heap-end*)

(defconstant +tag-mask+ 3)
(defconstant +tag-cons+ 1)
(defconstant +tag-heap+ 3)
(defconstant +word-size-in-bytes+ 8)

(defun check-space (size)
  (when (> (* size +word-size-in-bytes+)
	   (- *free-2* *free-1*))
    (error "Heap exhausted")))

(defun allocate-cons-cell ()
  (check-space 2)
  (prog1 (+ *free-1* +tag-cons+)
    (incf *free-1* (* 2 +word-size-in-bytes+))))

(defun allocate-heap-instance (size)
  (check-space (+ size 2))
  (decf *free-2* (* size +word-size-in-bytes+))
  (let ((header *free-1*))
    (incf *free-1* (* size +word-size-in-bytes+))
    (store-8-byte-word *free-2* (+ header +word-size-in-bytes+))
    (+ header +tag-heap+)))

(defun car (cons-cell)
  (assert (= +tag-cons+ (logand cons-cell +tag-mask+)))
  (load-8-byte-word (- cons-cell +tag-cons+)))

(defun (setf car) (value cons-cell)
  (assert (= +tag-cons+ (logand cons-cell +tag-mask+)))
  (store-8-byte-word
   value
   (- cons-cell +tag-cons+)))

(defun cdr (cons-cell)
  (assert (= +tag-cons+ (logand cons-cell +tag-mask+)))
  (load-8-byte-word (+ (- cons-cell +tag-cons+) +word-size-in-bytes+)))

(defun (setf cdr) (value cons-cell)
  (assert (= +tag-cons+ (logand cons-cell +tag-mask+)))
  (store-8-byte-word
   value
   (+ (- cons-cell +tag-cons+) +word-size-in-bytes+)))

(defun class (heap-instance)
  (assert (= +tag-heap+ (logand heap-instance +tag-mask+)))
  (load-8-byte-word (- heap-instance +tag-heap+)))

(defun (setf class) (value heap-instance)
  (assert (= +tag-heap+ (logand heap-instance +tag-mask+)))
  (store-8-byte-word
   value
   (- heap-instance +tag-heap+)))

(defun contents-vector (heap-instance)
  (assert (= +tag-heap+ (logand heap-instance +tag-mask+)))
  (load-8-byte-word (+ (- heap-instance +tag-heap+) +word-size-in-bytes+)))

(defun (setf contents-vector) (value heap-instance)
  (assert (= +tag-heap+ (logand heap-instance +tag-mask+)))
  (store-8-byte-word
   value
   (+ (- heap-instance +tag-heap+) +word-size-in-bytes+)))

(defun contents-element (heap-instance offset)
  (load-8-byte-word
   (+ (contents-vector heap-instance) (* offset +word-size-in-bytes+))))

(defun (setf contents-element) (value heap-instance offset)
  (store-8-byte-word
   value
   (+ (contents-vector heap-instance) (* offset +word-size-in-bytes+))))
