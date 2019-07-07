(cl:in-package #:sicl-hir-to-mir)

(defun find-element (array-location index size element-address-location instruction)
  (let ((rack-location (make-instance 'cleavir-ir:lexical-location
                         :name '#:rack-address)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:memref2-instruction
       :offset 3
       :inputs (list array-location)
       :outputs (list rack-location)
       :successors (list instruction))
     instruction)
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:unsigned-add-instruction
       :inputs (list rack-location (* index size))
       :outputs (list element-address-location)
       :successors (list instruction))
     instruction)))
