(in-package #:cleavir-escape)

(defun mark-dynamic-extent (initial-instruction &key liveness)
  (check-type initial-instruction cleavir-ir:enter-instruction)
  (let* ((s (make-instance 'escape))
         (dict (kildall s initial-instruction :liveness liveness)))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (inst)
       (when (and (typep inst 'cleavir-ir:allocation-mixin)
                  (dxable-p (find-in-pool s
                              (first (cleavir-ir:outputs inst))
                              (instruction-pool inst dict))))
         (setf (cleavir-ir:dynamic-extent-p inst) t)))
     initial-instruction)
    dict))
