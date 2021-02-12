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

(defmethod process-instruction
    ((instruction cleavir-ir:catch-instruction) lexical-locations)
  nil)

(defmethod process-instruction
    ((instruction cleavir-ir:bind-instruction) lexical-locations)
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
     lexical-locations)
  (let ((location (* (1+ (gethash to-lexical-location lexical-locations)) 8)))
    (cleavir-ir:insert-instruction-between
     (make-instance 'cleavir-ir:memset2-instruction
       :inputs (list *rbp*
                      (make-instance 'cleavir-ir:immediate-input
                        :value (- location))
                     from-register))
     instruction
     successor)))

(defun insert-memset-after
    (instruction
     from-register
     to-lexical-location
     lexical-locations)
  (assert (= (length (cleavir-ir:successors instruction)) 1))
  (insert-memset-between
   instruction
   (first (cleavir-ir:successors instruction))
   from-register
   to-lexical-location
   lexical-locations))

(defun insert-memsets-after
    (instruction
     from-register
     to-lexical-location
     lexical-locations)
  (loop for successor in (cleavir-ir:successors instruction)
        do (insert-memset-between
            instruction
            successor
            from-register
            to-lexical-location
            lexical-locations)))
