(in-package #:cleavir-escape)

(defclass escape
    (cleavir-kildall:iterate-mixin
     cleavir-liveness:live-after-mixin
     cleavir-kildall:start-everywhere-mixin
     cleavir-kildall:interfunction-mixin)
  ())
