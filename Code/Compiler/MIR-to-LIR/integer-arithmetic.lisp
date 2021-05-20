(cl:in-package #:sicl-mir-to-lir)

(defun process-arithmetic-instruction (instruction lexical-locations)
  (if (lexical-p (first (cleavir-ir:inputs instruction)))
      (insert-memref-before
       instruction
       (first (cleavir-ir:inputs instruction))
       *r11*
       lexical-locations)
      (cleavir-ir:insert-instruction-before
       (make-instance 'cleavir-ir:assignment-instruction
         :input (first (cleavir-ir:inputs instruction))
         :output *r11*)
       instruction))
  (if (null (rest (cleavir-ir:inputs instruction)))
      (setf (cleavir-ir:inputs instruction) (list *r11*))
      (if (lexical-p (second (cleavir-ir:inputs instruction)))
          (progn (insert-memref-before
                  instruction
                  (second (cleavir-ir:inputs instruction))
                  *r12*
                  lexical-locations)
                 (setf (cleavir-ir:inputs instruction) (list *r11* *r12*)))
          (setf (cleavir-ir:inputs instruction)
                (cons *r11* (rest (cleavir-ir:inputs instruction))))))
  (loop for successor in (cleavir-ir:successors instruction)
        do (insert-memset-between
            instruction
            successor
            *r11*
            (first (cleavir-ir:outputs instruction))
            lexical-locations))
  (setf (cleavir-ir:outputs instruction) (list *r11*)))

(defmethod process-instruction
    ((instruction cleavir-ir:unsigned-add-instruction) lexical-locations)
  (process-arithmetic-instruction instruction lexical-locations))

(defmethod process-instruction
    ((instruction cleavir-ir:unsigned-sub-instruction) lexical-locations)
  (process-arithmetic-instruction instruction lexical-locations))

(defmethod process-instruction
    ((instruction cleavir-ir:signed-add-instruction) lexical-locations)
  (process-arithmetic-instruction instruction lexical-locations))

(defmethod process-instruction
    ((instruction cleavir-ir:signed-sub-instruction) lexical-locations)
  (process-arithmetic-instruction instruction lexical-locations))

(defmethod process-instruction
    ((instruction cleavir-ir:shift-left-instruction) lexical-locations)
  (process-arithmetic-instruction instruction lexical-locations))

(defmethod process-instruction
    ((instruction cleavir-ir:logic-shift-right-instruction) lexical-locations)
  (process-arithmetic-instruction instruction lexical-locations))

(defmethod process-instruction
    ((instruction cleavir-ir:arithmetic-shift-right-instruction) lexical-locations)
  (process-arithmetic-instruction instruction lexical-locations))

(defmethod process-instruction
    ((instruction cleavir-ir:negate-instruction) lexical-locations)
  (process-arithmetic-instruction instruction lexical-locations))

(defun process-comparison-instruction (instruction lexical-locations)
  (let ((new-inputs
          (loop for input in (cleavir-ir:inputs instruction)
                for register in (list *r11* *r12*)
                if (lexical-p input)
                  do (insert-memref-before
                      instruction
                      input
                      register
                      lexical-locations)
                  and collect register
                else if (typep input 'cleavir-ir:immediate-input)
                       do (cleavir-ir:insert-instruction-before
                           (make-instance 'cleavir-ir:assignment-instruction
                             :input input
                             :output register)
                           instruction)
                       and collect register
                else
                  collect input)))
    (setf (cleavir-ir:inputs instruction) new-inputs)))

(defmethod process-instruction
    ((instruction cleavir-ir:unsigned-less-instruction) lexical-locations)
  (process-comparison-instruction instruction lexical-locations))

(defmethod process-instruction
    ((instruction cleavir-ir:unsigned-not-greater-instruction) lexical-locations)
  (process-comparison-instruction instruction lexical-locations))

(defmethod process-instruction
    ((instruction cleavir-ir:signed-less-instruction) lexical-locations)
  (process-comparison-instruction instruction lexical-locations))

(defmethod process-instruction
    ((instruction cleavir-ir:signed-not-greater-instruction) lexical-locations)
  (process-comparison-instruction instruction lexical-locations))

(defmethod process-instruction
    ((instruction cleavir-ir:equal-instruction) lexical-locations)
  (process-comparison-instruction instruction lexical-locations))

(defmethod process-instruction
    ((instruction cleavir-ir:eq-instruction) lexical-locations)
  (process-comparison-instruction instruction lexical-locations))
