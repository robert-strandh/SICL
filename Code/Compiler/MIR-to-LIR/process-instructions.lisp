(cl:in-package #:sicl-mir-to-lir)

(defun lexical-p (datum)
  (or (typep datum 'cleavir-ir:lexical-location)
      (typep datum 'cleavir-ir:raw-integer)))

(defgeneric process-instruction (instruction lexical-locations))

(defmethod process-instruction
    ((instruction cleavir-ir:enter-instruction) lexical-locations)
  (setf (cleavir-ir:outputs instruction) '()))

(defmethod process-instruction
    ((instruction cleavir-ir:nop-instruction) lexical-locations)
  nil)

(defmethod process-instruction
    ((instruction cleavir-ir:unreachable-instruction) lexical-locations)
  nil)

(defmethod process-instruction
    ((instruction sicl-ir:breakpoint-instruction) lexical-locations)
  nil)

;;; Return a list of instructions that, when executed, loads the
;;; address of LEXICAL-LOCATION into TO-REGISTER.
(defun load-address-of-lexical-location
    (lexical-location
     to-register
     lexical-locations)
  (let ((immediate-input
          (make-instance 'cleavir-ir:immediate-input
            :value (* (1+ (gethash lexical-location lexical-locations)) 8))))
    (list (make-instance 'cleavir-ir:assignment-instruction
            :input *rbp*
            :output to-register)
          (make-instance 'cleavir-ir:unsigned-sub-instruction
            :inputs (list to-register immediate-input)
            :output to-register))))

(defun insert-memref-before
    (instruction
     from-lexical-location
     to-register
     lexical-locations)
  (let ((location (* (1+ (gethash from-lexical-location lexical-locations)) 8)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:memref2-instruction
       :inputs (list *rbp*
                     (make-instance 'cleavir-ir:immediate-input
                       :value (- location)))
       :output to-register)
     instruction)))

(defun insert-memset-between
    (instruction
     successor
     from-register
     to-lexical-location
     scratch-register
     lexical-locations)
  (let ((memset (make-instance 'cleavir-ir:memset1-instruction
                  :inputs (list scratch-register from-register))))
    (cleavir-ir:insert-instruction-between
     memset
     instruction
     successor)
    (let ((load-instructions
            (load-address-of-lexical-location
             to-lexical-location scratch-register lexical-locations)))
      (loop for load-instruction in load-instructions
            do (cleavir-ir:insert-instruction-before
                load-instruction
                memset)))))

(defun insert-memset-after
    (instruction
     from-register
     to-lexical-location
     scratch-register
     lexical-locations)
  (assert (= (length (cleavir-ir:successors instruction)) 1))
  (insert-memset-between
   instruction
   (first (cleavir-ir:successors instruction))
   from-register
   to-lexical-location
   scratch-register
   lexical-locations))

(defun insert-memsets-after
    (instruction
     from-register
     to-lexical-location
     scratch-register
     lexical-locations)
  (loop for successor in (cleavir-ir:successors instruction)
        do (insert-memset-between
            instruction
            successor
            from-register
            to-lexical-location
            scratch-register
            lexical-locations)))

(defmethod process-instruction (instruction lexical-locations)
  (let ((inputs (cleavir-ir:inputs instruction))
        (outputs (cleavir-ir:outputs instruction)))
    (assert (<= 1 (length inputs) 2))
    (assert (<= 0 (length outputs) 1))
    (if (lexical-p (first inputs))
        (cond ((null outputs)
               ;; We use *r11* as an intermediate register
               (insert-memref-before
                instruction
                (first inputs)
                *r11*
                lexical-locations)
               (setf (first inputs) *r11*))
              ((lexical-p (first outputs))
               ;; We use *r11* as an intermediate register
               (insert-memref-before
                instruction
                (first inputs)
                *r11*
                lexical-locations)
               (setf (first inputs) *r11*)
               (insert-memsets-after
                instruction
                *r11*
                (first outputs)
                *rax*
                lexical-locations)
               (setf (first outputs) *r11*)
               (when (and (= (length inputs) 2)
                          (lexical-p (second inputs)))
                 (insert-memref-before
                  instruction
                  (second inputs)
                  *rax*
                  lexical-locations)
                 (setf (second inputs) *rax*)))
            ;; We use the output register as an intermediate register
            (t (insert-memref-before
                instruction
                (first inputs)
                (first outputs)
                lexical-locations)
               (setf (first inputs) (first outputs))
               (when (and (= (length inputs) 2)
                          (lexical-p (second inputs)))
                 (insert-memref-before
                  instruction
                  (second inputs)
                  *rax*
                  lexical-locations)
                 (setf (second inputs) *rax*))))
        (cond ((null outputs)
               nil)
              ((lexical-p (first outputs))
               (cleavir-ir:insert-instruction-before
                (make-instance 'cleavir-ir:assignment-instruction
                  :input (first inputs)
                  :output *r11*)
                instruction)
               (setf (first inputs) *r11*)
               (insert-memsets-after
                instruction
                *r11*
                (first outputs)
                *rax*
                lexical-locations)
               (setf (first outputs) *r11*)
               (when (and (= (length inputs) 2)
                          (lexical-p (second inputs)))
                 (insert-memref-before
                  instruction
                  (second inputs)
                  *rax*
                  lexical-locations)
                 (setf (second inputs) *rax*)))
              (t
               (unless (eq (first inputs) (first outputs))
                 (cleavir-ir:insert-instruction-before
                  (make-instance 'cleavir-ir:assignment-instruction
                    :input (first inputs)
                    :output (first outputs))
                  instruction)
                 (setf (first inputs) (first outputs))))))))
