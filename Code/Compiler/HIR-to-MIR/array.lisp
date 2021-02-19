(cl:in-package #:sicl-hir-to-mir)

(defun shift-count (element-type)
  (cond ((equal element-type 't) 3)
        ((equal element-type 'character) 3)
        ((equal element-type 'double-float) 3)
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
                      :address slot-location)))))

(defun process-bit-aref-instruction (instruction)
  (destructuring-bind (object-location index-location)
      (cleavir-ir:inputs instruction)
    (let* ((rack-location (find-rack instruction object-location))
           (first-slot-location (skip-rack-prefix instruction rack-location 2))
           (word-index-location (make-instance 'cleavir-ir:raw-integer :size 64))
           (word-element-location (make-instance 'cleavir-ir:raw-integer :size 64))
           (shifted-word-element-location (make-instance 'cleavir-ir:raw-integer :size 64))
           (masked-word-element-location (make-instance 'cleavir-ir:raw-integer :size 64))
           (index-mask-input (make-instance 'cleavir-ir:immediate-input :value 63))
           (immediate-input-1 (make-instance 'cleavir-ir:immediate-input :value 1))
           (shift-count-input (make-instance 'cleavir-ir:immediate-input :value 6))
           (bit-index-location (make-instance 'cleavir-ir:raw-integer :size 64)))
      (cleavir-ir:insert-instruction-before
       (make-instance 'cleavir-ir:logic-shift-right-instruction
         :shifted-input index-location
         :shift-count shift-count-input
         :output word-element-location
         :successor instruction)
       instruction)
      (let ((slot-location (compute-slot-location first-slot-location
                                                  word-index-location
                                                  instruction)))
        (cleavir-ir:insert-instruction-before
         (make-instance 'cleavir-ir:memref1-instruction
           :input slot-location
           :output word-element-location
           :successor instruction)
         instruction)
        (cleavir-ir:insert-instruction-before
         (make-instance 'cleavir-ir:bitwise-and-instruction
           :inputs (list index-location index-mask-input)
           :output bit-index-location
           :successor instruction)
         instruction)
        (cleavir-ir:insert-instruction-before
         (make-instance 'cleavir-ir:logic-shift-right-instruction
           :shifted-input word-element-location
           :shift-count bit-index-location
           :output shifted-word-element-location
           :successor instruction)
         instruction)
        (cleavir-ir:insert-instruction-before
         (make-instance 'cleavir-ir:bitwise-and-instruction
           :inputs (list shifted-word-element-location immediate-input-1)
           :output masked-word-element-location
           :successor instruction)
         instruction))
      (change-class instruction 'cleavir-ir:shift-left-instruction
                    :shifted-input masked-word-element-location
                    :shift-count immediate-input-1))))

(defmethod process-instruction
    (client (instruction cleavir-ir:aref-instruction) code-object)
  (let ((element-type (cleavir-ir:element-type instruction)))
    (cond ((equal element-type 'bit)
           (process-bit-aref-instruction instruction))
          (t
           (process-simple-aref-instruction instruction)))))

(defun process-simple-aset-instruction (instruction)
  (let* ((element-type (cleavir-ir:element-type instruction))
         (shift-count (shift-count element-type)))
    (destructuring-bind (object-location index-location value-location)
        (cleavir-ir:inputs instruction)
      (let* ((rack-location (find-rack instruction object-location))
             (first-slot-location (skip-rack-prefix instruction rack-location 2))
             (slot-offset-location (compute-slot-offset index-location
                                                        instruction
                                                        shift-count))
             (slot-location (compute-slot-location first-slot-location
                                                   slot-offset-location
                                                   instruction)))
        (change-class instruction 'cleavir-ir:memset1-instruction
                      :address slot-location
                      :value value-location)))))

(defun process-bit-aset-instruction (instruction)
  (destructuring-bind (object-location index-location value-location)
      (cleavir-ir:inputs instruction)
    (let* ((rack-location (find-rack instruction object-location))
           (first-slot-location (skip-rack-prefix instruction rack-location 2))
           (word-index-location (make-instance 'cleavir-ir:raw-integer :size 64))
           (word-element-location (make-instance 'cleavir-ir:raw-integer :size 64))
           (modified-word-element-location (make-instance 'cleavir-ir:raw-integer :size 64))
           (expanded-value-location (make-instance 'cleavir-ir:raw-integer :size 64))
           (shifted-value-location (make-instance 'cleavir-ir:raw-integer :size 64))
           (index-mask-input (make-instance 'cleavir-ir:immediate-input :value 63))
           (shift-count-input (make-instance 'cleavir-ir:immediate-input :value 6))
           (bit-index-location (make-instance 'cleavir-ir:raw-integer :size 64)))
      (cleavir-ir:insert-instruction-before
       (make-instance 'cleavir-ir:logic-shift-right-instruction
         :shifted-input index-location
         :shift-count shift-count-input
         :output word-element-location
         :successor instruction)
       instruction)
      (let ((slot-location (compute-slot-location first-slot-location
                                                  word-index-location
                                                  instruction)))
        (cleavir-ir:insert-instruction-before
         (make-instance 'cleavir-ir:memref1-instruction
           :input slot-location
           :output word-element-location
           :successor instruction)
         instruction)
        (cleavir-ir:insert-instruction-before
         (make-instance 'cleavir-ir:bitwise-and-instruction
           :inputs (list index-location index-mask-input)
           :output bit-index-location
           :successor instruction)
         instruction)
        (cleavir-ir:insert-instruction-before
         (make-instance 'cleavir-ir:assignment-instruction
           :input value-location
           :output expanded-value-location
           :successor instruction)
         instruction)
        (cleavir-ir:insert-instruction-before
         (make-instance 'cleavir-ir:shift-left-instruction
           :shifted-input expanded-value-location
           :shift-count bit-index-location
           :output shifted-value-location
           :successor instruction)
         instruction)
        (cleavir-ir:insert-instruction-before
         (make-instance 'cleavir-ir:bitwise-or-instruction
           :inputs (list word-element-location shifted-value-location)
           :output modified-word-element-location
           :successor instruction)
         instruction)
      (change-class instruction 'cleavir-ir:memset1-instruction
                    :address word-element-location
                    :value modified-word-element-location)))))

(defmethod process-instruction
    (client (instruction cleavir-ir:aset-instruction) code-object)
  (let ((element-type (cleavir-ir:element-type instruction)))
    (cond ((equal element-type 'bit)
           (process-bit-aset-instruction instruction))
          (t
           (process-simple-aset-instruction instruction)))))
