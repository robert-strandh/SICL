(cl:in-package #:sicl-hir-to-mir)

(defun find-element (array-location index size element-address-location instruction)
  (let ((rack-location (find-rack instruction array-location)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:unsigned-add-instruction
       :inputs (list rack-location (* index size))
       :outputs (list element-address-location)
       :successors (list instruction))
     instruction)))
