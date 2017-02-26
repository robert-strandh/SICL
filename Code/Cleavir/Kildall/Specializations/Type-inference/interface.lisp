(in-package #:cleavir-kildall-type-inference)

(defun infer-types (initial-instruction &key draw)
  (let* ((s (make-instance 'type-inference))
         (d (cleavir-kildall:kildall s initial-instruction)))
    (when draw
      (cleavir-kildall-graphviz:draw-flowchart-with-inputs
       initial-instruction draw s d))
    d))
