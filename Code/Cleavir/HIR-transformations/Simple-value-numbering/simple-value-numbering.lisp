(cl:in-package #:cleavir-simple-value-numbering)

;;; We use instances of this class to designate values.
(defclass value-designator () ())

;;; This variable holds and EQ hash table mapping constants to value
;;; designators.
(defvar *constant-designators*)

;;  LocalWords:  designator designators
