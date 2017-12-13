(in-package #:cleavir-value-numbering)

(defmethod cleavir-kildall-graphviz:draw-object ((s value-numbering) object)
  (princ-to-string object))

(defun number-values (initial-instruction &key draw liveness)
  (let* ((traverse (make-instance 'value-numbering))
         (dict (cleavir-kildall:kildall traverse initial-instruction :liveness liveness)))
    (when draw
      (cleavir-kildall-graphviz:draw-flowchart-with-inputs
       initial-instruction draw traverse dict))
    dict))
