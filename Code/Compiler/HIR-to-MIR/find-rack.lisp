(cl:in-package #:sicl-hir-to-Mir)

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
;;; reference to the header of a general instance G, insert and
;;; instruction J before I.  The instruction J is a memory reference
;;; instruction that outputs a reference to the rack of G and stores
;;; that reference in a fresh lexical location L.  The lexical
;;; location L is returned.  Recall that the reference to the rack is
;;; a tagged pointer, and its tag is #b111.

(defun find-rack (instruction general-instance-location)
  (let ((offset-input (make-instance 'cleavir-ir:immediate-input :value 3))
        (rack-location (make-instance 'cleavir-ir:lexical-location
                         :name '#:rack-location)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:memref2-instruction
       :base-address general-instance-location
       :offset offset-input
       :output rack-location
       :successor instruction)
     instruction)
    rack-location))
