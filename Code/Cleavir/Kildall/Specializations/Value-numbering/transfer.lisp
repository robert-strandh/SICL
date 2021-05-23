(in-package #:cleavir-value-numbering)

;;; Generic instruction: Assign all outputs new numbers, pass them along.
(defmethod cleavir-kildall:transfer ((s value-numbering) instruction)
  (let ((outputs (cleavir-ir:outputs instruction)))
    (cleavir-kildall:with-pool-reader s instruction from
      (dolist (succ (cleavir-ir:successors instruction))
        (cleavir-kildall:copy s succ variable from
                              ((outputs (list (cons instruction (position variable outputs)))))
                              ())))))

;;; Assignment: Output gets the input number.
(defmethod cleavir-kildall:transfer ((s value-numbering) (instruction cleavir-ir:assignment-instruction))
  (cleavir-kildall:with-pool-reader s instruction from
    (let* ((input (first (cleavir-ir:inputs instruction)))
           (input-number (from input))
           (output (first (cleavir-ir:outputs instruction)))
           (succ (cleavir-ir:first-successor instruction)))
      (cleavir-kildall:copy s succ variable from
                            ()
                            ((output input-number))))))

;;; Could add more for e.g. box/unbox, values.
