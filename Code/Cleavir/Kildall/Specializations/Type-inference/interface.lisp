(in-package #:cleavir-kildall-type-inference)

(defun infer-types (initial-instruction environment
                    &key prune draw)
  (let* ((s (make-instance 'type-inference :env environment))
         (d (cleavir-kildall:kildall s initial-instruction)))
    (when prune
      (prune-typeqs initial-instruction s d))
    (when draw
      (cleavir-kildall-graphviz:draw-flowchart-with-inputs
       initial-instruction draw s d))
    d))
