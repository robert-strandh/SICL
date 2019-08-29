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
