(cl:in-package sicl-boot-phase-5)

(defmethod sicl-hir-evaluator:instruction-thunk
    ((client client)
     (instruction cleavir-ir:standard-object-class-of-instruction)
     lexical-environment)
  (sicl-hir-evaluator:make-thunk
      (client instruction lexical-environment :inputs 1 :outputs 1)
    (setf (sicl-hir-evaluator:output 0)
          (let ((object (sicl-hir-evaluator:input 0)))
            (cond ((typep object 'sicl-boot::header)
                   (slot-value object 'sicl-boot::%class))
                  ((null object)
                   (env:find-class client (sicl-boot:environment client)
                                   'sicl-boot::host-null))
                  ((symbolp object)
                   (env:find-class client (sicl-boot:environment client)
                                   'sicl-boot::host-symbol))
                  ((stringp object)
                   (env:find-class client (sicl-boot:environment client)
                                   'sicl-boot::host-string))
                  ((packagep object)
                   (env:find-class client (sicl-boot:environment client)
                                   'sicl-boot::host-package))
                  ((functionp object)
                   (env:find-class client (sicl-boot:environment client)
                                   'function))
                  ((integerp object)
                   (env:find-class
                    client (sicl-boot:environment client)
                    (cond ((< object (- (expt 2 62)))
                           'sicl-arithmetic:negative-bignum)
                          ((>= object (expt 2 62))
                           'sicl-arithmetic:positive-bignum)
                          (t
                           'fixnum))))
                  (t
                   (error "Class of ~s asked for in E5" object)))))
    (sicl-hir-evaluator:successor 0)))

(defmethod sicl-hir-evaluator:instruction-thunk
    ((client client)
     (instruction cleavir-ir:aref-instruction)
     lexical-environment)
  (let ((element-type (cleavir-ir:element-type instruction)))
    (sicl-hir-evaluator:make-thunk (client instruction lexical-environment :inputs 2 :outputs 1)
      (setf (sicl-hir-evaluator:output 0)
            (cond ((typep (sicl-hir-evaluator:input 0) 'array)
                   (row-major-aref (sicl-hir-evaluator:input 0)
                                   (sicl-hir-evaluator:input 1)))
                  ((equal element-type '(unsigned-byte 8))
                   (let ((rack (slot-value (sicl-hir-evaluator:input 0) 'sicl-boot::%rack)))
                     (multiple-value-bind (word-index byte-index)
                         (floor (sicl-hir-evaluator:input 1) 8)
                       (ldb (byte 8 (* 8 byte-index)) (aref rack (+ word-index 5))))))
                  ((equal element-type '(unsigned-byte 32))
                   (let ((rack (slot-value (sicl-hir-evaluator:input 0) 'sicl-boot::%rack)))
                     (multiple-value-bind (word-index half-word-index)
                         (floor (sicl-hir-evaluator:input 1) 2)
                       (ldb (byte 32 (* 32 half-word-index)) (aref rack (+ word-index 5))))))
                  ((eq element-type 'character)
                   (let ((rack (slot-value (sicl-hir-evaluator:input 0) 'sicl-boot::%rack)))
                     (multiple-value-bind (word-index half-word-index)
                         (floor (sicl-hir-evaluator:input 1) 2)
                       (code-char (ldb (byte 32 (* 32 half-word-index)) (aref rack (+ word-index 5)))))))
                  (t
                   (let ((rack (slot-value (sicl-hir-evaluator:input 0) 'sicl-boot::%rack)))
                     (aref rack (+ (sicl-hir-evaluator:input 1) 5))))))
      (sicl-hir-evaluator:successor 0))))

(defmethod sicl-hir-evaluator:instruction-thunk
    ((client client)
     (instruction cleavir-ir:aset-instruction)
     lexical-environment)
  (let ((element-type (cleavir-ir:element-type instruction)))
    (sicl-hir-evaluator:make-thunk (client instruction lexical-environment :inputs 3 :outputs 0)
      (cond ((typep (sicl-hir-evaluator:input 0) 'array)
             (setf (row-major-aref (sicl-hir-evaluator:input 0)
                                   (sicl-hir-evaluator:input 1))
                   (sicl-hir-evaluator:input 2)))
            ((equal element-type '(unsigned-byte 8))
             (let ((rack (slot-value (sicl-hir-evaluator:input 0) 'sicl-boot::%rack)))
               (multiple-value-bind (word-index byte-index)
                   (floor (sicl-hir-evaluator:input 1) 8)
                 (setf (ldb (byte 8 (* 8 byte-index)) (aref rack (+ word-index 5)))
                       (sicl-hir-evaluator:input 2)))))
            ((equal element-type '(unsigned-byte 32))
             (let ((rack (slot-value (sicl-hir-evaluator:input 0) 'sicl-boot::%rack)))
               (multiple-value-bind (word-index half-word-index)
                   (floor (sicl-hir-evaluator:input 1) 2)
                 (setf (ldb (byte 32 (* 32 half-word-index)) (aref rack (+ word-index 5)))
                       (sicl-hir-evaluator:input 2)))))
            ((eq element-type 'character)
             (let ((rack (slot-value (sicl-hir-evaluator:input 0) 'sicl-boot::%rack)))
               (multiple-value-bind (word-index half-word-index)
                   (floor (sicl-hir-evaluator:input 1) 2)
                 (setf (ldb (byte 32 (* 32 half-word-index)) (aref rack (+ word-index 5)))
                       (char-code (sicl-hir-evaluator:input 2))))))
            (t
             (let ((rack (slot-value (sicl-hir-evaluator:input 0) 'sicl-boot::%rack)))
               (setf (aref rack (+ (sicl-hir-evaluator:input 1) 5))
                     (sicl-hir-evaluator:input 2)))))
      (sicl-hir-evaluator:successor 0))))

(defmethod sicl-hir-evaluator:instruction-thunk
    ((client client)
     (instruction sicl-ir:rack-instruction)
     lexical-environment)
  (sicl-hir-evaluator:make-thunk
      (client instruction lexical-environment :inputs 1 :outputs 1)
    (setf (sicl-hir-evaluator:output 0)
          (let ((object (sicl-hir-evaluator:input 0)))
            (slot-value object 'sicl-boot::%rack)))
    (sicl-hir-evaluator:successor 0)))

(defmethod sicl-hir-evaluator:instruction-thunk
    ((client client)
     (instruction sicl-ir:set-rack-instruction)
     lexical-environment)
  (sicl-hir-evaluator:make-thunk
      (client instruction lexical-environment :inputs 2 :outputs 0)
    (let ((object (sicl-hir-evaluator:input 0))
          (rack (sicl-hir-evaluator:input 1)))
      (setf (slot-value object 'sicl-boot::%rack)
            rack))
    (sicl-hir-evaluator:successor 0)))
