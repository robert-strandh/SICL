(cl:in-package #:cleavir-type-inference)

(defvar *dictionary*)

(declaim (inline make-dictionary arc-bag))

(defun make-dictionary ()
  (make-hash-table :test #'equal))

(defun arc-bag (from to dictionary)
  (gethash (cons from to) dictionary))
(defun (setf arc-bag) (new-value from to dictionary)
  (setf (gethash (cons from to) dictionary) new-value))

(defun instruction-input (instruction dictionary)
  (apply #'bag-join
         (mapcar (lambda (pred)
                   (arc-bag pred instruction dictionary))
                 (cleavir-ir:predecessors instruction))))

(defvar *work-list*)
