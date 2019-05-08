(cl:in-package #:sicl-hir-to-cl)

(defgeneric translate (client instruction context))

(defmethod translate (client (instruction cleavir-ir:assignment-instruction) context)
  (let* ((input (first (cleavir-ir:inputs instruction)))
         (output (first (cleavir-ir:outputs instruction))))
    `((setq ,(cleavir-ir:name output)
            ,(if (typep input 'cleavir-ir:constant-input)
                 `',(cleavir-ir:value input)
                 (cleavir-ir:name input))))))

(defmethod translate (client (instruction cleavir-ir:funcall-instruction) context)
  (let ((inputs (cleavir-ir:inputs instruction)))
     `((setq ,(values-location context)
             (multiple-value-list
              (funcall ,@(mapcar #'cleavir-ir:name inputs)))))))

(defmethod translate (client (instruction cleavir-ir:return-instruction) context)
  `((return-from ,(block-name context)
      (apply #'values ,(values-location context)))))

(defmethod translate (client (instruction cleavir-ir:nop-instruction) context)
  (declare (ignore context))
  '())

(defmethod translate-final-instruction (client
                                        (instruction cleavir-ir:eq-instruction)
                                        context
                                        dynamic-environment-stack)
  (destructuring-bind (input1 input2)
      (cleavir-ir:inputs instruction)
    (destructuring-bind (successor1 successor2)
        (cleavir-ir:successors instruction)
      (let ((then-tag (tag-of-basic-block (basic-block-of-leader successor1)))
            (else-tag (tag-of-basic-block (basic-block-of-leader successor2))))
        `((if (eq ,input1 ,input2)
              (progn ,@(compute-dynamic-environment-pops
                        successor1 dynamic-environment-stack)
                     (go ,then-tag))
              (progn ,@(compute-dynamic-environment-pops
                        successor2 dynamic-environment-stack)
                     (go ,else-tag))))))))

(defmethod translate-final-instruction (client
                                        (instruction cleavir-ir:consp-instruction)
                                        context
                                        dynamic-environment-stack)
  (let ((input (first (cleavir-ir:inputs instruction))))
    (destructuring-bind (successor1 successor2)
        (cleavir-ir:successors instruction)
      (let ((then-tag (tag-of-basic-block (basic-block-of-leader successor1)))
            (else-tag (tag-of-basic-block (basic-block-of-leader successor2))))
        `((if (consp ,input)
              (progn ,@(compute-dynamic-environment-pops
                        successor1 dynamic-environment-stack)
                     (go ,then-tag))
              (progn ,@(compute-dynamic-environment-pops
                        successor2 dynamic-environment-stack)
                     (go ,else-tag))))))))

(defmethod translate-final-instruction (client
                                        (instruction cleavir-ir:fixnump-instruction)
                                        context
                                        dynamic-environment-stack)
  (let ((input (first (cleavir-ir:inputs instruction))))
    (destructuring-bind (successor1 successor2)
        (cleavir-ir:successors instruction)
      (let ((then-tag (tag-of-basic-block (basic-block-of-leader successor1)))
            (else-tag (tag-of-basic-block (basic-block-of-leader successor2))))
        `((if (typep ,input 'fixnum)
              (progn ,@(compute-dynamic-environment-pops
                        successor1 dynamic-environment-stack)
                     (go ,then-tag))
              (progn ,@(compute-dynamic-environment-pops
                        successor2 dynamic-environment-stack)
                     (go ,else-tag))))))))

(defmethod translate-final-instruction (client
                                        (instruction cleavir-ir:characterp-instruction)
                                        context
                                        dynamic-environment-stack)
  (let ((input (first (cleavir-ir:inputs instruction))))
    (destructuring-bind (successor1 successor2)
        (cleavir-ir:successors instruction)
      (let ((then-tag (tag-of-basic-block (basic-block-of-leader successor1)))
            (else-tag (tag-of-basic-block (basic-block-of-leader successor2))))
        `((if (characterp ,input)
              (progn ,@(compute-dynamic-environment-pops
                        successor1 dynamic-environment-stack)
                     (go ,then-tag))
              (progn ,@(compute-dynamic-environment-pops
                        successor2 dynamic-environment-stack)
                     (go ,else-tag))))))))
