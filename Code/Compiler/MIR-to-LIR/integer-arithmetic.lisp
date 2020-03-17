(cl:in-package #:sicl-mir-to-lir)

(defun process-arithmetic-instruction (instruction lexical-locations)
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
                else
                  collect input)))
    (setf (cleavir-ir:inputs instruction) new-inputs)
    (loop for successor in (cleavir-ir:successors instruction)
          do (insert-memset-between
              instruction
              successor
              *r11*
              (first (cleavir-ir:outputs instruction))
              lexical-locations))
    (setf (cleavir-ir:outputs instruction) (list *r11*))))

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
