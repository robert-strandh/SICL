(in-package #:cleavir-kildall-escape)

(defclass escape (cleavir-kildall:reverse-spread-traverse
                  cleavir-kildall-liveness:reverse-filtered-pool-mixin
                  cleavir-kildall:reverse-traverse-interfunction)
  ())

(defmethod cleavir-kildall:entry-pool ((s escape) i)
  (declare (ignore i))
  (cleavir-kildall:empty-map-pool))
