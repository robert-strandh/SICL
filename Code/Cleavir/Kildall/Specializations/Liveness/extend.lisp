(in-package #:cleavir-kildall-liveness)

(defclass filtered-pool-mixin (cleavir-kildall:map-pool-mixin)
  ((%liveness :initarg :liveness :accessor fp-liveness)))

(defclass forward-filtered-pool-mixin
    (filtered-pool-mixin cleavir-kildall:forward-traverse)
  ())

(defclass reverse-filtered-pool-mixin
    (filtered-pool-mixin cleavir-kildall:reverse-traverse)
  ())

(defmethod cleavir-kildall:process-instruction :around
    ((s forward-filtered-pool-mixin) instruction new-in)
  (let ((filtered
          (cleavir-kildall:alist->map-pool
           (loop for live in (cleavir-kildall-liveness:live-before
                              (fp-liveness s) instruction)
                 collect (cons live
                               (cleavir-kildall:find-in-pool
                                s live new-in))))))
    (call-next-method s instruction filtered)))

(defmethod cleavir-kildall:process-instruction :around
    ((s reverse-filtered-pool-mixin) instruction new-in)
  (let ((filtered
          (cleavir-kildall:alist->map-pool
           (loop for live in (cleavir-kildall-liveness:live-after
                              (fp-liveness s) instruction)
                 collect (cons live
                               (cleavir-kildall:find-in-pool
                                s live new-in))))))
    (call-next-method s instruction filtered)))
