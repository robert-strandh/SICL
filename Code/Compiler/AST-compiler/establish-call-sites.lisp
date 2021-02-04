(cl:in-package #:sicl-compiler)

(defun establish-call-sites (code-object)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (when (typep instruction 'sicl-ir:named-call-mixin)
       (let ((function-cell-cell (list nil))
             name)
         (etypecase instruction
           (cleavir-ir:named-call-instruction
            (setf name (cleavir-ir:callee-name instruction))
            (change-class instruction
                          'sicl-ir:named-call-instruction
                          :function-cell-cell function-cell-cell)))
         (push (make-instance 'call-site
                 :name name
                 :instruction instruction)
               (call-sites code-object)))))
   (ir code-object)))
