(in-package #:cleavir-escape)

;;; :draw specifies a file to write out an annotated .dot to.
(defun mark-dynamic-extent (initial-instruction
                            &key draw
                              (liveness
                               (cleavir-liveness:liveness
                                initial-instruction)))
  (check-type initial-instruction cleavir-ir:enter-instruction)
  (let* ((s (make-instance 'escape :liveness liveness))
         (d (cleavir-kildall:kildall s initial-instruction)))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (inst)
       (when (typep inst 'cleavir-ir:allocation-mixin)
         (let ((dxness
                 (cleavir-kildall:find-in-pool s
                  (first (cleavir-ir:outputs inst))
                  (cleavir-kildall:instruction-pool inst d))))
           (when (dxable-p dxness)
             (setf (cleavir-ir:dynamic-extent-p inst) t)))))
     initial-instruction)
    (when draw
      (cleavir-kildall-graphviz:draw-flowchart-with-outputs
       initial-instruction draw s d))
    (values d s)))
