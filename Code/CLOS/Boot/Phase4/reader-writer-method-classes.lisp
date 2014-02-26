(cl:in-package #:sicl-clos)

(setf *standard-reader-method*
  (find-target-class 'standard-reader-method))

(setf *standard-writer-method*
  (find-target-class 'standard-writer-method))
