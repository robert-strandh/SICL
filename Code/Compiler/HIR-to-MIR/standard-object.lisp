(cl:in-package #:sicl-hir-to-mir)

;;; The rack of every standard object has a prefix that contains
;;; information about the rack, as opposed to the contents of the
;;; object it represents.  For a standard object that is not an
;;; array, that prefix consists of 2 words, where the first word is
;;; the stamp and the second words contains a list of effective slot
;;; definitions.  For simple arrays, the prefix contains the stamp and
;;; a list of dimensions.  Other arrays contain information about fill
;;; pointers and displacement.  This function takes an integer
;;; representing the number of words in this prefix, and it returns an
;;; immediate input with an offset corresponding to those words,
;;; i.e. the argument multiplied by 8 minus the tag value of a rack
;;; pointer, which is 7.
(defun make-rack-prefix-offset (prefix-size)
  (make-instance 'cleavir-ir:immediate-input :value (- (* prefix-size 8) 7)))

(defmethod  process-instruction
    (client (instruction cleavir-ir:nook-read-instruction))
  (destructuring-bind (object-location slot-number-location)
      (cleavir-ir:inputs instruction)
    (let* ((rack-location (find-rack instruction object-location))
           (first-slot-location (skip-rack-prefix instruction rack-location 2))
           (slot-offset-location (compute-slot-offset slot-number-location
                                                      instruction
                                                      3))
           (slot-location (compute-slot-location first-slot-location
                                                 slot-offset-location
                                                 instruction)))
      (change-class instruction 'cleavir-ir:memref1-instruction
                    :inputs (list slot-location)
                    :outputs (cleavir-ir:outputs instruction)))))

(defmethod  process-instruction
    (client (instruction cleavir-ir:nook-write-instruction))
  (destructuring-bind (object-location slot-number-location value-location)
      (cleavir-ir:inputs instruction)
    (let* ((rack-location (find-rack instruction object-location))
           (first-slot-location (skip-rack-prefix instruction rack-location 2))
           (slot-offset-location (compute-slot-offset slot-number-location
                                                      instruction
                                                      3))
           (slot-location (compute-slot-location first-slot-location
                                                 slot-offset-location
                                                 instruction)))
      (change-class instruction 'cleavir-ir:memset1-instruction
                    :inputs (list slot-location value-location)))))
