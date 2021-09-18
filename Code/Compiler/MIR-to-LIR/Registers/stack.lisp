(cl:in-package #:sicl-x86-64-registers)

(defconstant +stack-slot-size+ 8)

(defun save-to-stack-instruction (from-register to-stack-slot)
  (make-instance 'cleavir-ir:memset2-instruction
    :inputs (list *rsp*
                  (make-instance 'cleavir-ir:immediate-input
                    :value (* to-stack-slot +stack-slot-size+))
                  from-register)))

(defun load-from-stack-instruction (from-stack-slot to-register)
 (make-instance 'cleavir-ir:memref2-instruction
   :inputs (list *rsp*
                 (make-instance 'cleavir-ir:immediate-input
                   :value (* from-stack-slot +stack-slot-size+)))
   :outputs (list to-register)))

(defun save-to-location-instruction (from-register location)
  (etypecase location
    (cleavir-ir:register-location
     (make-instance 'cleavir-ir:assignment-instruction
       :inputs  (list from-register)
       :outputs (list location)))
    (cleavir-ir:stack-location
     (save-to-stack-instruction from-register (cleavir-ir:offset location)))))

(defun load-from-location-instruction (location to-register)
  (etypecase location
    (cleavir-ir:register-location
     (make-instance 'cleavir-ir:assignment-instruction
       :inputs  (list location)
       :outputs (list to-register)))
    (cleavir-ir:stack-location
     (load-from-stack-instruction (cleavir-ir:offset location) to-register))))
