(in-package #:cleavir-liveness)

(defclass liveness-mixin () ())

(defclass live-before-mixin (liveness-mixin
                             cleavir-kildall:alist-pool-mixin)
  ())
(defclass live-after-mixin (liveness-mixin
                            cleavir-kildall:alist-pool-mixin)
  ())

(defvar *kildall-liveness*)

(defmethod cleavir-kildall:kildall :around
    ((s liveness-mixin) initial-instruction &key liveness)
  (let ((*kildall-liveness*
          ;; not a default argument because it's convenient to
          ;; have :liveness nil do this too, and -p is a hassle.
          (or liveness (liveness initial-instruction))))
    (call-next-method)))

(defmethod cleavir-kildall:instruction-variables
    ((s live-before-mixin) instruction)
  (live-before *kildall-liveness* instruction))

(defmethod cleavir-kildall:instruction-variables
    ((s live-after-mixin) instruction)
  (live-after *kildall-liveness* instruction))
