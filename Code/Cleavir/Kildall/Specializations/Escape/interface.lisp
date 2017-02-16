(in-package #:cleavir-kildall-escape)

(defun mark-one-function (enter dict)
  (cleavir-ir:map-instructions-locally
   (lambda (inst)
     (when (typep inst 'cleavir-ir:allocation-mixin)
       (let ((dxness
               (find-in-pool (first (cleavir-ir:outputs inst))
                             (gethash inst dict))))
         (when (dxable-p dxness)
           (setf (cleavir-ir:dynamic-extent-p inst) t)))))
   enter))

;;; for debug
(defun analyze (initial-instruction)
  (let ((s (make-instance 'escape :enter initial-instruction)))
    (cleavir-kildall:kildall s initial-instruction)))

(defun mark-dynamic-extent (initial-instruction)
  (check-type initial-instruction cleavir-ir:enter-instruction)
  (let* ((s (make-instance 'escape :enter initial-instruction))
         (d (cleavir-kildall:kildall s initial-instruction)))
    (setf (cleavir-kildall:dictionary s) d)
    (cleavir-kildall:map-tree #'mark-one-function s)))
