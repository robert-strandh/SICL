(cl:in-package #:sicl-hir-to-mir)

;;; Given an instruction I and a lexical location containing a
;;; reference to the header of a general instance G, insert and
;;; instruction J before I.  The instruction J is a memory reference
;;; instruction that outputs a reference to the rack of G and stores
;;; that reference in a fresh lexical location L.  The lexical
;;; location L is returned.

(defun find-rack (instruction general-instance-location)
  (let ((offset-input (make-instance 'cleavir-ir:immediate-input :value 3))
        (rack-location (make-instance 'cleavir-ir:lexical-location
                         :name #:rack-location)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:memref2-instruction
       :base-address general-instance-location
       :offset offset-input
       :output rack-location
       :successor instruction)
     instruction)
    rack-location))
