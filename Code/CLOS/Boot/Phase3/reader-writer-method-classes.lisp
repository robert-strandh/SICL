(cl:in-package #:sicl-clos)

(setf *standard-reader-method*
  (find-bridge-class 'standard-reader-method))

(setf *standard-writer-method*
  (find-bridge-class 'standard-writer-method))
