(cl:in-package #:sicl-mir-to-lir)

(defmethod finish-lir-for-instruction
    ((instruction cleavir-ir:initialize-return-values-instruction))
  (change-class instruction
                'cleavir-ir:assignment-instruction
                :outputs (list x86-64:*return-value-count*)))

(defmethod finish-lir-for-instruction
    ((instruction cleavir-ir:compute-return-value-count-instruction))
  (change-class instruction
                'cleavir-ir:assignment-instruction
                :inputs (list x86-64:*return-value-count*)))

;;; Return values that aren't stored in registers will be stored in
;;; some THREAD object that hasn't been designed yet.
(defun nag-for-thread-object ()
  (warn "Supposed to do something with the THREAD object for the other return values..."))

;;; We assume that the index is always constant if it will be read
;;; from a register.  MULTIPLE-VALUE-CALL appears to be the only
;;; construct which uses variable value indices, and it unrolls so
;;; that the registers are loaded with constant indexes.
(defmethod finish-lir-for-instruction
    ((instruction cleavir-ir:set-return-value-instruction))
  (destructuring-bind (index value)
      (cleavir-ir:inputs instruction)
    (cond
      ((and (typep index 'cleavir-ir:immediate-input)
            (< (cleavir-ir:value index)
               (* 2 (length x86-64:*return-registers*))))
       (change-class instruction
                     'cleavir-ir:assignment-instruction
                     :inputs (list value)
                     :outputs (list (nth (floor (cleavir-ir:value index) 2)
                                         x86-64:*return-registers*))))
      (t
       (nag-for-thread-object)))))

(defmethod finish-lir-for-instruction
    ((instruction cleavir-ir:return-value-instruction))
  (destructuring-bind (index)
      (cleavir-ir:inputs instruction)
    (cond
      ((and (typep index 'cleavir-ir:immediate-input)
            (< (cleavir-ir:value index)
               (* 2 (length x86-64:*return-registers*))))
       (change-class instruction
                     'cleavir-ir:assignment-instruction
                     :inputs (list (nth (floor (cleavir-ir:value index) 2)
                                        x86-64:*return-registers*))))
      (t
       (nag-for-thread-object)))))
