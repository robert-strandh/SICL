(in-package #:cleavir-escape)

(defclass escape (cleavir-kildall:reverse-spread-traverse
                  cleavir-liveness:reverse-filtered-pool-mixin
                  cleavir-kildall:reverse-traverse-interfunction)
  ())

(defmethod cleavir-kildall:entry-pool ((s escape) i)
  (declare (ignore i))
  (cleavir-kildall:empty-map-pool))
