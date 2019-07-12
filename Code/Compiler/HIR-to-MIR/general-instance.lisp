(cl:in-package #:sicl-hir-to-mir)

;;; Given a location L containing a slot number, and and in struction
;;; I, insert an instruction J before I, such that J computes a new
;;; location M containing the contents of L shifted left by 8
;;; positions to obtain a slot offset.  The location M is returned.
(defun compute-slot-offset (slot-number-location instruction)
  (let ((offset-location (make-instance 'cleavir-ir:lexical-location
                           :name '#:slot-offset-location))
        (shift-count-input (make-instance 'cleavir-ir:immediate-input
                             :value 8)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:shift-left-instruction
       :shifted-input offset-location
       :shift-count shift-count-input
       :successor instruction)
     instruction)
    offset-location))

;;; This function computes the location of a slot, except that the
;;; rack prefix is not included in the resulting value.  It takes two
;;; locations, the first one contains the address of the beginning of
;;; the rack, and the second one contains a slot offset, where offset
;;; 0 indicates the first slot.  It returns a location containing the
;;; sum of the values of the two input locations.
(defun compute-slot-location (rack-location slot-offset-location instruction)
  (let ((slot-location (make-instance 'cleavir-ir:lexical-location
                         :name '#:slot-location)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:unsigned-add-instruction
       :augend rack-location
       :addend slot-offset-location
       :output slot-location
       :successor instruction)
     instruction)
    slot-location))

;;; The rack of every general instance has a prefix that contains
;;; information about the rack, as opposed to the contents of the
;;; object it represents.  For a general instance that is not an
;;; array, that prefix consists of 2 words, where the first word is
;;; the stamp and the second words contains a list of effective slot
;;; definitions.  For simple arrays, the prefix contains the stamp and
;;; a list of dimensions.  Other arrays contain information about fill
;;; pointers and displacement.  This function takes an integer
;;; representing the number of words in this prefix, and it returns an
;;; immediate input corresponding to those words, i.e. the argument
;;; multiplied by 8.
(defun make-rack-prefix-offset (prefix-size)
  (make-instance 'cleavir-ir:immediate-input :value (* prefix-size 8)))

(defmethod  process-instruction (client
                                 (instruction cleavir-ir:slot-read-instruction))
  (destructuring-bind (object-location slot-number-location)
      (cleavir-ir:inputs instruction)
    (let* ((rack-location (find-rack instruction object-location))
           (slot-offset-location (compute-slot-offset slot-number-location instruction))
           (slot-location (compute-slot-location rack-location slot-offset-location instruction))
           (rack-prefix-input (make-rack-prefix-offset 2)))
      (change-class instruction 'cleavir-ir:memref2-instruction
                    :base-address slot-location
                    :offset rack-prefix-input))))

(defmethod  process-instruction (client
                                 (instruction cleavir-ir:slot-write-instruction))
  (destructuring-bind (object-location slot-number value-location)
      (cleavir-ir:inputs instruction)
    (let* ((rack-location (find-rack instruction object-location))
           (slot-offset-location (compute-slot-offset slot-number-location instruction))
           (slot-location (compute-slot-location rack-location slot-offset-location instruction))
           (rack-prefix-input (make-rack-prefix-offset 2)))
      (change-class instruction 'cleavir-ir:memset2-instruction
                    :base-address slot-location
                    :offset rack-prefix-location
                    :value value-location))))
