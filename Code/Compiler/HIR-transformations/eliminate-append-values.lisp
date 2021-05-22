(cl:in-package #:sicl-hir-transformations)

;;; When we start, the CAR of the cell in the VALUES-LOCATION contains
;;; a pointer to the cell where we should start adding values.  So the
;;; first instruction is to load the CAR of the cell in the
;;; VALUES-LOCATION into the CELL-LOCATION.
(defun make-initial-instructions
    (successor
     values-location
     value-count-location
     cell-location
     dynamic-environment-location)
  (make-instance 'cleavir-ir:compute-return-value-count-instruction
    :output value-count-location
    :dynamic-environment-location dynamic-environment-location
    :successor
    (make-instance 'cleavir-ir:car-instruction
      :input values-location
      :output cell-location
      :dynamic-environment-location dynamic-environment-location
      :successor successor)))

(defun make-incrementation-instruction
    (successor index-location dynamic-environment-location)
  (make-instance 'cleavir-ir:unsigned-add-instruction
    :augend index-location
    :addend (make-instance 'cleavir-ir:constant-input
              :value 1)
    :output index-location
    :dynamic-environment-location dynamic-environment-location
    :successor successor))

;;; The final instruction sets the CAR of the cell in the
;;; VALUES-LOCATION to the CELL-LOCATION, thereby either indicating
;;; the start cell for the next APPEND-VALUES-INSTRUCTION, or
;;; indicating the end of the list of arguments to supply to the
;;; function invoked by MULTIPLE-VALUE-CALL.
(defun make-final-instruction
    (successor values-location cell-location dynamic-environment-location)
  (make-instance 'cleavir-ir:rplaca-instruction
    :inputs (list values-location cell-location)
    :dynamic-environment-location dynamic-environment-location
    :successor successor))

(defun make-test-step
    (true-successor
     false-successor
     value-count-location
     index-input
     cell-location
     dynamic-environment-location)
  (let ((temp-location (make-instance 'cleavir-ir:lexical-location
                         :name (gensym "temp"))))
    ;; Check whether there are more values to access.
    (make-instance 'cleavir-ir:unsigned-less-instruction
      :inputs (list index-input value-count-location)
      :dynamic-environment-location dynamic-environment-location
      :successors
      (list
       ;; This branch is taken when there are more values to access.
       ;; Access the value indicated by INDEX-INPUT, and put it in a
       ;; temporary location.
       (make-instance 'cleavir-ir:return-value-instruction
         :input index-input
         :output temp-location
         :dynamic-environment-location dynamic-environment-location
         :successor
         ;; The put the value in the temporary location in the CAR of
         ;; the cell indicated by CELL-LOCATION.
         (make-instance 'cleavir-ir:rplaca-instruction
           :inputs (list cell-location temp-location)
           :dynamic-environment-location dynamic-environment-location
           :successor
           ;; Finally, advance the CELL-LOCATION to the next cell in
           ;; the list.
           (make-instance 'cleavir-ir:cdr-instruction
             :input cell-location
             :output cell-location
             :dynamic-environment-location dynamic-environment-location
             :successor true-successor)))
       false-successor))))

(defun make-loop
    (successor
     value-count-location
     cell-location
     dynamic-environment-location)
  (let* ((index-location (make-instance 'cleavir-ir:lexical-location
                           :name (gensym "index")))
         (nop (make-instance 'cleavir-ir:nop-instruction))
         (increment (make-incrementation-instruction
                     nop
                     index-location
                     dynamic-environment-location))
         (test (make-test-step
                increment
                successor
                value-count-location
                index-location
                cell-location
                dynamic-environment-location)))
    (rplaca (cleavir-ir:successors increment) test)
    (make-instance 'cleavir-ir:assignment-instruction
      :input (make-instance 'cleavir-ir:constant-input
               :value 5)
      :output index-location
      :dynamic-environment-location dynamic-environment-location
      :successor test)))

;;; This function is charged with computing a sequence of instructions
;;; that are meant to replace a single APPEND-VALUES-INSTRUCTION.  It
;;; returns the first instruction and the last instruction of the
;;; sequence as two values.
(defun make-replacement
    (successor
     values-location
     dynamic-environment-location)
  (let* ((cell-location (make-instance 'cleavir-ir:lexical-location
                          :name (gensym "cell")))
         (value-count-location (make-instance 'cleavir-ir:lexical-location
                                 :name (gensym "VC")))
         (final (make-final-instruction
                 successor
                 values-location
                 cell-location
                 dynamic-environment-location))
         (true-branch (make-loop
                       final
                       value-count-location
                       cell-location
                       dynamic-environment-location)))
    (loop for i downfrom 4 to 0
          do (setf true-branch
                   (make-test-step
                    true-branch
                    final
                    value-count-location
                    (make-instance 'cleavir-ir:constant-input
                      :value i)
                    cell-location
                    dynamic-environment-location)))
    (values (make-initial-instructions
             true-branch
             values-location
             value-count-location
             cell-location
             dynamic-environment-location)
            final)))

(defun eliminate-append-values-instruction (instruction)
  (let* ((values-location (first (cleavir-ir:outputs instruction)))
         (dynamic-environment-location
           (cleavir-ir:dynamic-environment-location instruction))
         (successor (first (cleavir-ir:successors instruction))))
    (multiple-value-bind (first last)
        (make-replacement
         successor
         values-location
         dynamic-environment-location)
      (setf (cleavir-ir:predecessors first)
            (cleavir-ir:predecessors instruction))
      (loop for predecessor in (cleavir-ir:predecessors first)
            do (setf (cleavir-ir:successors predecessor)
                     (substitute first
                                 instruction
                                 (cleavir-ir:successors predecessor))))
      (setf (cleavir-ir:predecessors successor)
            (substitute last
                        instruction
                        (cleavir-ir:predecessors successor))))))

(defun eliminate-append-values-instructions (initial-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (when (typep instruction 'cleavir-ir:append-values-instruction)
       (eliminate-append-values-instruction instruction)))
   initial-instruction))
