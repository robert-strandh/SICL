(cl:in-package #:sicl-hir-to-cl)

(defgeneric translate (client instruction context))

(defun translate-input (input)
  (if (typep input 'cleavir-ir:constant-input)
      `',(cleavir-ir:value input)
      (cleavir-ir:name input)))

(defmethod translate (client (instruction cleavir-ir:assignment-instruction) context)
  (let* ((input (first (cleavir-ir:inputs instruction)))
         (output (first (cleavir-ir:outputs instruction))))
    `((setq ,(cleavir-ir:name output)
            ,(translate-input input)))))

(defmethod translate (client (instruction cleavir-ir:funcall-instruction) context)
  (let ((inputs (cleavir-ir:inputs instruction)))
     `((setq ,(values-location context)
             (multiple-value-list
              (funcall ,@(mapcar #'translate-input inputs)))))))

(defmethod translate (client (instruction cleavir-ir:nop-instruction) context)
  (declare (ignore context))
  '())

(defmethod translate (client (instruction cleavir-ir:bind-instruction) context)
  (let ((successor (first (cleavir-ir:successors instruction))))
    `((push (make-instance 'special-variable-entry
              :name `',(first (cleavir-ir:inputs instruction))
              :value (cleavir-ir:name (second (cleavir-ir:inputs instruction))))
            *dynamic-environment*)
      (tagbody (go ,(tag-of-basic-block (basic-block-of-leader successor)))
         ,@(loop with dynamic-environment-output-location
                   = (first (cleavir-ir:outputs instruction))
                 with basic-blocks
                   = (basic-blocks-in-dynamic-environment
                      dynamic-environment-output-location)
                 for basic-block in basic-blocks
                 collect (tag-of-basic-block basic-block)
                 append (let ((*dynamic-environment-stack*
                                (cons dynamic-environment-output-location
                                      *dynamic-environment-stack*)))
                          (translate-basic-block
                           client
                           basic-block
                           context)))))))

(defmethod translate-final-instruction (client
                                        (instruction cleavir-ir:eq-instruction)
                                        context)
  (destructuring-bind (input1 input2)
      (cleavir-ir:inputs instruction)
    (destructuring-bind (successor1 successor2)
        (cleavir-ir:successors instruction)
      (let ((then-tag (tag-of-basic-block (basic-block-of-leader successor1)))
            (else-tag (tag-of-basic-block (basic-block-of-leader successor2))))
        `((if (eq ,input1 ,input2)
              (progn ,@(compute-dynamic-environment-pops successor1)
                     (go ,then-tag))
              (progn ,@(compute-dynamic-environment-pops successor2)
                     (go ,else-tag))))))))

(defmethod translate-final-instruction (client
                                        (instruction cleavir-ir:consp-instruction)
                                        context)
  (let ((input (first (cleavir-ir:inputs instruction))))
    (destructuring-bind (successor1 successor2)
        (cleavir-ir:successors instruction)
      (let ((then-tag (tag-of-basic-block (basic-block-of-leader successor1)))
            (else-tag (tag-of-basic-block (basic-block-of-leader successor2))))
        `((if (consp ,input)
              (progn ,@(compute-dynamic-environment-pops successor1)
                     (go ,then-tag))
              (progn ,@(compute-dynamic-environment-pops successor2)
                     (go ,else-tag))))))))

(defmethod translate-final-instruction (client
                                        (instruction cleavir-ir:fixnump-instruction)
                                        context)
  (let ((input (first (cleavir-ir:inputs instruction))))
    (destructuring-bind (successor1 successor2)
        (cleavir-ir:successors instruction)
      (let ((then-tag (tag-of-basic-block (basic-block-of-leader successor1)))
            (else-tag (tag-of-basic-block (basic-block-of-leader successor2))))
        `((if (typep ,input 'fixnum)
              (progn ,@(compute-dynamic-environment-pops successor1)
                     (go ,then-tag))
              (progn ,@(compute-dynamic-environment-pops successor2)
                     (go ,else-tag))))))))

(defmethod translate-final-instruction (client
                                        (instruction cleavir-ir:characterp-instruction)
                                        context)
  (let ((input (first (cleavir-ir:inputs instruction))))
    (destructuring-bind (successor1 successor2)
        (cleavir-ir:successors instruction)
      (let ((then-tag (tag-of-basic-block (basic-block-of-leader successor1)))
            (else-tag (tag-of-basic-block (basic-block-of-leader successor2))))
        `((if (characterp ,input)
              (progn ,@(compute-dynamic-environment-pops successor1)
                     (go ,then-tag))
              (progn ,@(compute-dynamic-environment-pops successor2)
                     (go ,else-tag))))))))

(defmethod translate-final-instruction (client
                                        (instruction cleavir-ir:unwind-instruction)
                                        constext)
  `((unwind ,(cleavir-ir:name (first (cleavir-ir:inputs instruction)))
            ,(cleavir-ir:unwind-index instruction))))
