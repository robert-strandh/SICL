(cl:in-package #:cleavir-cst-to-ast-test)

(defclass environment () ())

(defmethod cleavir-environment:optimize-info ((environment environment))
  (make-instance 'cleavir-environment:optimize-info
    :optimize '((speed 0) (compilation-speed 0) (space 0) (debug 3) (safety 3))
    :policy '()))
