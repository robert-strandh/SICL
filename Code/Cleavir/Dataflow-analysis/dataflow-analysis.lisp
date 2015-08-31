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
