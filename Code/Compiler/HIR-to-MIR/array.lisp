(cl:in-package #:sicl-hir-to-mir)

(defun find-element (array-location index size element-address-location instruction)
  (let ((rack-location (find-rack instruction array-location)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:unsigned-add-instruction
       :inputs (list rack-location (* index size))
       :outputs (list element-address-location)
       :successors (list instruction))
     instruction)))

(defun shift-count (element-type)
  (cond ((equal element-type 't) 3)
        ((equal element-type 'character) 3)
        ((equal element-type '(signed-byte 8)) 0)
        ((equal element-type '(unsigned-byte 8)) 0)
        ((equal element-type '(signed-byte 16)) 1)
        ((equal element-type '(unsigned-byte 16)) 1)
        ((equal element-type '(signed-byte 32)) 2)
        ((equal element-type '(unsigned-byte 32)) 2)
        (t (error "not simple ~s" element-type))))

(defun process-simple-aref-instruction (instruction)
  (let* ((element-type (cleavir-ir:element-type instruction))
         (shift-count (shift-count element-type)))
    (destructuring-bind (object-location index-location)
        (cleavir-ir:inputs instruction)
      (let* ((rack-location (find-rack instruction object-location))
             (first-slot-location (skip-rack-prefix instruction rack-location 2))
             (slot-offset-location (compute-slot-offset index-location
                                                        instruction
                                                        shift-count))
             (slot-location (compute-slot-location first-slot-location
                                                   slot-offset-location
                                                   instruction)))
        (change-class instruction 'cleavir-ir:memref1-instruction
                      :input slot-location
                      :outputs (cleavir-ir:outputs instruction))))))
