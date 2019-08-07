(cl:in-package #:sicl-hir-to-cl)

(defgeneric translate (client instruction context))

(defmethod translate :around (client instruction context)
  (cons `(setq source
               (compute-source-info
                source ',(cleavir-ast-to-hir:origin instruction)))
        (call-next-method)))

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
  (let* ((values-location (first (cleavir-ir:outputs instruction)))
         (name (gethash values-location (values-locations context)))
         (inputs (cleavir-ir:inputs instruction)))
    `((setq *dynamic-environment*
            ,(cleavir-ir:name (cleavir-ir:dynamic-environment-location instruction)))
      (setq ,name
             (multiple-value-list
              (funcall ,@(mapcar #'translate-input inputs)))))))

(defmethod translate (client (instruction cleavir-ir:nop-instruction) context)
  (declare (ignore context))
  '())

(defmethod translate-final-instruction (client (instruction cleavir-ir:bind-instruction) context)
  (let* ((successor (first (cleavir-ir:successors instruction)))
         (dynamic-environment-output-location
           (first (cleavir-ir:outputs instruction)))
         (basic-blocks (basic-blocks-in-dynamic-environment
                        dynamic-environment-output-location)))
    `((let ((entry (make-instance 'special-variable-entry
                     :name ,(cleavir-ir:name (first (cleavir-ir:inputs instruction)))
                     :value ,(cleavir-ir:name (second (cleavir-ir:inputs instruction))))))
        (setq ,(cleavir-ir:name (cleavir-ir:dynamic-environment-output instruction))
              (cons entry
                    ,(cleavir-ir:name (cleavir-ir:dynamic-environment-location instruction)))))
      (tagbody (go ,(tag-of-basic-block (basic-block-of-leader successor)))
         ,@(loop for basic-block in basic-blocks
                 for tag = (tag-of-basic-block basic-block)
                 collect tag
                 append (translate-basic-block
                         client
                         basic-block
                         context))))))

(defmethod translate-final-instruction (client
                                        (instruction cleavir-ir:eq-instruction)
                                        context)
  (destructuring-bind (input1 input2)
      (cleavir-ir:inputs instruction)
    (destructuring-bind (successor1 successor2)
        (cleavir-ir:successors instruction)
      (let ((then-tag (tag-of-basic-block (basic-block-of-leader successor1)))
            (else-tag (tag-of-basic-block (basic-block-of-leader successor2))))
        `((if (eq ,(translate-input input1) ,(translate-input input2))
              (progn (go ,then-tag))
              (progn (go ,else-tag))))))))

(defmethod translate-final-instruction (client
                                        (instruction cleavir-ir:consp-instruction)
                                        context)
  (let ((input (first (cleavir-ir:inputs instruction))))
    (destructuring-bind (successor1 successor2)
        (cleavir-ir:successors instruction)
      (let ((then-tag (tag-of-basic-block (basic-block-of-leader successor1)))
            (else-tag (tag-of-basic-block (basic-block-of-leader successor2))))
        `((if (consp ,(translate-input input))
              (progn (go ,then-tag))
              (progn (go ,else-tag))))))))

(defmethod translate-final-instruction (client
                                        (instruction cleavir-ir:fixnump-instruction)
                                        context)
  (let ((input (first (cleavir-ir:inputs instruction))))
    (destructuring-bind (successor1 successor2)
        (cleavir-ir:successors instruction)
      (let ((then-tag (tag-of-basic-block (basic-block-of-leader successor1)))
            (else-tag (tag-of-basic-block (basic-block-of-leader successor2))))
        `((if (typep ,(translate-input input) 'fixnum)
              (progn (go ,then-tag))
              (progn (go ,else-tag))))))))

(defmethod translate-final-instruction (client
                                        (instruction cleavir-ir:characterp-instruction)
                                        context)
  (let ((input (first (cleavir-ir:inputs instruction))))
    (destructuring-bind (successor1 successor2)
        (cleavir-ir:successors instruction)
      (let ((then-tag (tag-of-basic-block (basic-block-of-leader successor1)))
            (else-tag (tag-of-basic-block (basic-block-of-leader successor2))))
        `((if (characterp ,(translate-input input))
              (progn (go ,then-tag))
              (progn (go ,else-tag))))))))

(defmethod translate-final-instruction (client
                                        (instruction cleavir-ir:unwind-instruction)
                                        constext)
  `((unwind ,(cleavir-ir:name (first (cleavir-ir:inputs instruction)))
            ,(cleavir-ir:unwind-index instruction)
            ,(cleavir-ir:name (cleavir-ir:dynamic-environment-location instruction)))))
