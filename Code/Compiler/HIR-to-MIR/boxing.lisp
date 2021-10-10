(cl:in-package #:sicl-hir-to-mir)

(defun box-unsigned-integer (instruction)
  (let ((word-location (make-instance 'cleavir-ir:raw-integer :size 64))
        (shift-count-input (make-instance 'cleavir-ir:immediate-input :value 1)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:assignment-instruction
       :inputs (cleavir-ir:inputs instruction)
       :output word-location
       :successor instruction)
     instruction)
    (change-class instruction 'cleavir-ir:shift-left-instruction
                  :shifted-input word-location
                  :shift-count shift-count-input)))

(defun box-signed-integer (instruction)
  (let ((word-location (make-instance 'cleavir-ir:raw-integer :size 64))
        (shift-count-input (make-instance 'cleavir-ir:immediate-input :value 1)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:sign-extend-instruction
       :inputs (cleavir-ir:inputs instruction)
       :output word-location
       :successor instruction)
     instruction)
    (change-class instruction 'cleavir-ir:shift-left-instruction
                  :shifted-input word-location
                  :shift-count shift-count-input)))

(defun box-single-float (instruction)
  (let ((word-location-1 (make-instance 'cleavir-ir:raw-integer :size 64))
        (word-location-2 (make-instance 'cleavir-ir:raw-integer :size 64))
        (shift-count-input (make-instance 'cleavir-ir:immediate-input :value 32))
        (tag-input (make-instance 'cleavir-ir:immediate-input :value #b10011)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:assignment-instruction
       :inputs (cleavir-ir:inputs instruction)
       :output word-location-1
       :successor instruction)
     instruction)
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:shift-left-instruction
       :shifted-input word-location-1
       :shift-count shift-count-input
       :output word-location-2
       :successor instruction)
     instruction)
    (change-class instruction 'cleavir-ir:bitwise-or-instruction
                  :inputs (list word-location-2 tag-input))))

(defmethod process-instruction
    (client (instruction cleavir-ir:box-instruction))
  (let ((element-type (cleavir-ir:element-type instruction)))
    (cond ((member element-type
                   '(bit
                     (unsigned-byte 8)
                     (unsigned-byte 16)
                     (unsigned-byte 32))
                   :test #'equal)
           (box-unsigned-integer instruction))
          ((member element-type
                   '((signed-byte 8)
                     (signed-byte 16)
                     (signed-byte 32))
                   :test #'equal)
           (box-signed-integer instruction))
          ((eq element-type 'single-float)
           (box-single-float instruction))
          ((eq element-type 'double-float)
           (let* ((dynamic-environment
                    (cleavir-ir:dynamic-environment-location instruction))
                  (double-float-constant-location
                    (make-instance 'cleavir-ir:lexical-location
                      :name (gensym)))
                  (object-location
                    (first (cleavir-ir:outputs instruction)))
                  (call-instruction
                    (make-instance 'cleavir-ir:named-call-instruction
                      :dynamic-environment-location dynamic-environment
                      :callee-name 'make-instance
                      :input double-float-constant-location)))
             (cleavir-ir:insert-instruction-before
              (let ((literal 'double-float))
                (sicl-compiler:ensure-literal
                 (sicl-compiler:constants *code-object*) literal)
                (make-instance 'cleavir-ir:load-literal-instruction
                  :dynamic-environment-location dynamic-environment
                  :output double-float-constant-location
                  :location-info (list literal)))
              instruction)
             (cleavir-ir:insert-instruction-before call-instruction instruction)
             (cleavir-ir:insert-instruction-before
              (make-instance 'cleavir-ir:return-value-instruction
                :dynamic-environment-location dynamic-environment
                :input (make-instance 'cleavir-ir:immediate-input :value 0)
                :output object-location)
              instruction)
             (process-nook-write-instruction
              instruction
              object-location
              (make-instance 'cleavir-ir:immediate-input
                             :value 3)
              (first (cleavir-ir:inputs instruction)))))
          ((or (equal element-type '(signed-byte 64))
               (equal element-type '(unsigned-byte 64)))
           (let ((dynamic-environment
                   (cleavir-ir:dynamic-environment-location instruction))
                 (message-constant-location
                   (make-instance 'cleavir-ir:lexical-location
                     :name (gensym))))
             (cleavir-ir:insert-instruction-before
              (let ((literal "Can't box ((un)signed-byte 64) yet"))
                (sicl-compiler:ensure-literal
                 (sicl-compiler:constants *code-object*) literal)
                (make-instance 'cleavir-ir:load-literal-instruction
                  :dynamic-environment-location dynamic-environment
                  :output message-constant-location
                  :location-info (list literal)))
              instruction)
             (change-class instruction 'cleavir-ir:named-call-instruction
               :dynamic-environment-location dynamic-environment
               :callee-name 'error
               :inputs (list message-constant-location)
               :outputs '()
               :successors (list (make-instance 'cleavir-ir:unreachable-instruction
                                   :dynamic-environment-location dynamic-environment)))))
          (t
           (error "Don't know how to box ~s" element-type)))))

(defun unbox-integer (instruction)
  (let ((word-location (make-instance 'cleavir-ir:raw-integer :size 64))
        (shift-count-input (make-instance 'cleavir-ir:immediate-input :value 1)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:arithmetic-shift-right-instruction
       :shifted-input (first (cleavir-ir:inputs instruction))
       :shift-count shift-count-input
       :output word-location
       :successor instruction)
     instruction)
    (change-class instruction 'cleavir-ir:assignment-instruction
                  :inputs (list word-location))))

(defun unbox-single-float (instruction)
  (let ((word-location (make-instance 'cleavir-ir:raw-integer :size 64))
        (shift-count-input (make-instance 'cleavir-ir:immediate-input :value 32)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:arithmetic-shift-right-instruction
       :shifted-input (first (cleavir-ir:inputs instruction))
       :shift-count shift-count-input
       :output word-location
       :successor instruction)
     instruction)
    (change-class instruction 'cleavir-ir:assignment-instruction
                  :inputs (list word-location))))

(defmethod process-instruction
    (client (instruction cleavir-ir:unbox-instruction))
  (let ((element-type (cleavir-ir:element-type instruction)))
    (cond ((member element-type
                   '(bit
                     (unsigned-byte 8)
                     (unsigned-byte 16)
                     (unsigned-byte 32)
                     (signed-byte 8)
                     (signed-byte 16)
                     (signed-byte 32))
                   :test #'equal)
           (unbox-integer instruction))
          ((eq element-type 'single-float)
           (unbox-single-float instruction))
          ((eq element-type 'double-float)
           (process-nook-read-instruction
            instruction
            (first (cleavir-ir:inputs instruction))
            (make-instance 'cleavir-ir:immediate-input
                           :value 3)))
          ((or (equal element-type '(signed-byte 64))
               (equal element-type '(unsigned-byte 64)))
           ;; FIXME: What does it mean to unbox a (signed-byte 64)?
           (process-nook-read-instruction
            instruction
            (first (cleavir-ir:inputs instruction))
            (make-instance 'cleavir-ir:immediate-input
                           :value 3)))
          (t
           (error "Don't know how to unbox ~s" element-type)))))
