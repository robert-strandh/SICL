(cl:in-package #:cleavir-dataflow-analysis)

(defclass operation ()
  ((%instruction :initarg :instruction :reader instruction)
   (%inputs :initform '() :initarg inputs :accessor inputs)
   (%outputs :initform '() :initarg outputs :accessor outputs)))

(defclass dataflow ()
  ((%operations :initarg :operations :accessor operations)))
