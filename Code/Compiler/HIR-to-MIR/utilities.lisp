(cl:in-package #:sicl-hir-to-mir)

;;; Insert either an UNSIGNED-ADD-INSTRUCTION or an
;;; UNSIGNED-SUB-INSTRUCTION (depending on the sign of OFFSET) before
;;; INSTRUCTION.  The input to the inserted instruction is
;;; TAGGED-POINTER-LOCATION and an immediate input created from
;;; OFFSET.  The output is a raw pointer location which is returned
;;; from this function.

(defun raw-pointer-from-tagged-pointer (instruction tagged-pointer-location offset)
  (let* ((class-name (if (minusp offset)
                         'cleavir-ir:unsigned-sub-instruction
                         'cleavir-ir:unsigned-add-instruction))
         (offset-input (make-instance 'cleavir-ir:immediate-input :value (abs offset)))
         (raw-pointer-location (make-instance 'cleavir-ir:raw-integer :size 64)))
    (cleavir-ir:insert-instruction-before
     (make-instance class-name
       :inputs (list tagged-pointer-location offset-input)
       :output raw-pointer-location
       :successor instruction)
     instruction)
    raw-pointer-location))

;;; Given an instruction I and a lexical location containing a
;;; reference to the header of a standard object G, insert and
;;; instruction J before I.  The instruction J is a memory reference
;;; instruction that outputs a reference to the rack of G and stores
;;; that reference in a fresh lexical location L.  The lexical
;;; location L is returned.  Recall that the reference to the rack is
;;; a tagged pointer, and its tag is #b111.

(defun find-rack (instruction standard-object-location)
  (let ((rack-location (make-instance 'cleavir-ir:lexical-location
                         :name '#:rack-location)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:memref2-instruction
       :inputs (list standard-object-location
                     (make-instance 'cleavir-ir:immediate-input
                       :value 3))
       :output rack-location
       :dynamic-environment-location
       (cleavir-ir:dynamic-environment-location instruction)
       :successor instruction)
     instruction)
    rack-location))

(defun skip-rack-prefix (instruction rack-location prefix-size)
  (let ((addend (make-instance 'cleavir-ir:immediate-input
                  :value (- (* prefix-size 8) 7)))
        (raw-pointer-location (make-instance 'cleavir-ir:raw-integer :size 64)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:unsigned-add-instruction
       :augend rack-location
       :addend addend
       :output raw-pointer-location
       :successors (list instruction instruction))
     instruction)
    raw-pointer-location))

;;; Given a location L containing a slot number, and and instruction
;;; I, insert an instruction J before I, such that J computes a new
;;; location M containing the contents of L shifted left by
;;; SHIFT-COUNT positions to obtain a slot offset.  The location M is
;;; returned.
(defun compute-slot-offset (slot-number-location instruction shift-count)
  (let ((offset-location (make-instance 'cleavir-ir:raw-integer :size 64))
        (shift-count-input (make-instance 'cleavir-ir:immediate-input
                             :value shift-count)))
    (cleavir-ir:insert-instruction-before
     (if (typep slot-number-location 'cleavir-ir:immediate-input)
         (make-instance 'cleavir-ir:assignment-instruction
           :input (make-instance 'cleavir-ir:immediate-input
                    :value (ash (cleavir-ir:value slot-number-location) shift-count))
           :output offset-location)
         (make-instance 'cleavir-ir:shift-left-instruction
           :shifted-input slot-number-location
           :shift-count shift-count-input
           :output offset-location))
     instruction)
    offset-location))

;;; This function computes the location of a slot.  It takes two
;;; locations, the first one contains the address of the first slot,
;;; and the second one contains a slot offset, where offset 0
;;; indicates the first slot.  It returns a location containing the
;;; sum of the values of the two input locations.
(defun compute-slot-location (first-slot-location slot-offset-location instruction)
  (let ((slot-location (make-instance 'cleavir-ir:lexical-location
                         :name '#:slot-location)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:unsigned-add-instruction
       :augend first-slot-location
       :addend slot-offset-location
       :output slot-location
       :successor instruction)
     instruction)
    slot-location))
