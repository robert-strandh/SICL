(cl:in-package #:cleavir-dataflow-analysis)

(defclass port ()
  (;; The operation to which this port belongs.
   (%operation :initarg :operation :reader operation)
   ;; The datum that this port is using or defining.
   (%datum :initarg :datum :reader datum)))

(defclass input (port)
  ((%outputs :initform '() :initarg outputs :accessor outputs)))

(defclass output (port)
  ((%inputs :initform '() :initarg inputs :accessor inputs)))

(defclass operation ()
  ((%instruction :initarg :instruction :reader instruction)
   (%inputs :initform '() :initarg inputs :reader inputs)
   (%outputs :initform '() :initarg outputs :reader outputs)))

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
       (let* ((operation (make-instance 'operation
			   :instruction instruction))
	      (inputs (loop for input in (cleavir-ir:inputs instruction)
			    collect (make-instance 'input
				      :operation operation
				      :datum input)))
	      (outputs (loop for output in (cleavir-ir:outputs instruction)
			     collect (make-instance 'output
				       :operation operation
				       :datum output))))
	 (reinitialize-instance operation
				:inputs inputs
				:outputs outputs)
	 (setf (gethash instruction (operations result))
	       operation)))
     initial-instruction)
    result))
