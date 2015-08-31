(cl:in-package #:cleavir-dataflow-analysis)

(defclass operation ()
  ((%instruction :initarg :instruction :reader instruction)
   (%inputs :initform '() :initarg inputs :accessor inputs)
   (%outputs :initform '() :initarg outputs :accessor outputs)))

(defclass dataflow ()
  (;; This hash table maps each instruction to its corresponding
   ;; operation
   (%operations :initform (make-hash-table :test #'eq)
		:initarg :operations
		:accessor operations)))

(defun dataflow-analysis (initial-instruction)
  (let ((result (make-instance 'dataflow)))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (setf (gethash instruction (operations result))
	     (make-instance 'operation
	       :instruction instruction)))
     initial-instruction)
    result))
