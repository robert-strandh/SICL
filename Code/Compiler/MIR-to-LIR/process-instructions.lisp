(cl:in-package #:sicl-mir-to-lir)

(defgeneric process-instruction (instruction lexical-locations))

(defmethod process-instruction (instruction lexical-locations)
  (declare (ignore lexical-locations))
  (error "Don't know how to process instruction ~s" instruction))

(defmethod process-instruction
    ((instruction cleavir-ir:assignment-instruction) lexical-locations)
  (let ((dynamic-environment-location
          (cleavir-ir:dynamic-environment-location instruction))
        (input (first (cleavir-ir:inputs instruction)))
        (output (first (cleavir-ir:outputs instruction))))
    (cond ((and (typep input 'cleavir-ir:register-location)
                (typep output 'cleavir-ir:register-location))
           ;; Leave it as it is
           nil)
          ((and (or (typep input 'cleavir-ir:register-location)
                    (typep input 'cleavir-ir:immediate-input))
                (typep output 'cleavir-ir:lexical-location))
           (let ((immediate-input
                   (make-instance 'cleavir-ir:immediate-input
                     :value (+ (gethash output lexical-locations) 8))))
             (cleavir-ir:insert-instruction-before
              (make-instance 'cleavir-ir:assignment-instruction
                :input *rbp*
                :output *r11*
                :dynamic-environment-location dynamic-environment-location)
              instruction)
             (cleavir-ir:insert-instruction-before
              (make-instance 'cleavir-ir:unsigned-sub-instruction
                :inputs (list *r11* immediate-input)
                :output *r11*
                :dynamic-environment-location dynamic-environment-location)
              instruction)
             (change-class instruction
                           'cleavir-ir:memset1-instruction
                           :inputs (list *r11* input)
                           :outputs '())))
          (t (error "Can't handle input ~s and output ~s" input output)))))

(defun insert-memref-before
    (instruction lexical-location register lexical-locations)
  (let ((immediate-input
          (make-instance 'cleavir-ir:immediate-input
            :value (+ (gethash lexical-location lexical-locations) 8)))
        (dynamic-environment-location
          (cleavir-ir:dynamic-environment-location instruction)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:assignment-instruction
       :input *rbp*
       :output *r11*
       :dynamic-environment-location dynamic-environment-location)
     instruction)
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:unsigned-sub-instruction
       :inputs (list *r11* immediate-input)
       :output *r11*
       :dynamic-environment-location dynamic-environment-location)
     instruction)
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:memref1-instruction
       :input *r11*
       :output register
       :dynamic-environment-location dynamic-environment-location)
     instruction)))

(defun insert-memset-after
    (instruction register lexical-location lexical-locations)
  (let ((immediate-input
          (make-instance 'cleavir-ir:immediate-input
            :value (+ (gethash lexical-location lexical-locations) 8)))
        (dynamic-environment-location
          (cleavir-ir:dynamic-environment-location instruction)))
    (cleavir-ir:insert-instruction-after
     (make-instance 'cleavir-ir:memset1-instruction
       :inputs (list *r11* register)
       :output register
       :dynamic-environment-location dynamic-environment-location)
     instruction)
    (cleavir-ir:insert-instruction-after
     (make-instance 'cleavir-ir:unsigned-sub-instruction
       :inputs (list *r11* immediate-input)
       :output *r11*
       :dynamic-environment-location dynamic-environment-location)
     instruction)
    (cleavir-ir:insert-instruction-after
     (make-instance 'cleavir-ir:assignment-instruction
       :input *rbp*
       :output *r11*
       :dynamic-environment-location dynamic-environment-location)
     instruction)))

(defun process-comparison (instruction lexical-locations)
  (destructuring-bind (input1 input2)
      (cleavir-ir:inputs instruction)
    (when (typep input1 'cleavir-ir:lexical-location)
      (insert-memref-before input1 *r11* input1 lexical-locations)
      (setf (first (cleavir-ir:inputs instruction)) *r11*))
    (when (typep input2 'cleavir-ir:lexical-location)
      (insert-memref-before input1 *r12* input1 lexical-locations)
      (setf (second (cleavir-ir:inputs instruction)) *r12*))))

(defmethod process-instruction
    ((instruction cleavir-ir:unsigned-less-instruction) lexical-locations)
  (process-comparison instruction lexical-locations))

(defmethod process-instruction
    ((instruction cleavir-ir:unsigned-not-greater-instruction) lexical-locations)
  (process-comparison instruction lexical-locations))

(defmethod process-instruction
    ((instruction cleavir-ir:eq-instruction) lexical-locations)
  (process-comparison instruction lexical-locations))
