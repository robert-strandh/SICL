(cl:in-package #:sicl-compiler)

(defun establish-call-sites (code-object)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (typecase instruction
       (cleavir-ir:named-call-instruction
        (change-class instruction
                      'sicl-ir:named-call-instruction
                      :function-cell-cell (list nil))
        (push (make-instance 'call-site
                :name (cleavir-ir:callee-name instruction)
                :instruction instruction)
              (call-sites code-object)))
       (cleavir-ir:catch-instruction
        (change-class instruction
                      'sicl-ir:catch-instruction
                      :function-cell-cell (list nil))
        (push (make-instance 'call-site
                :name 'sicl-run-time:augment-with-block/tagbody-entry
                :instruction instruction)
              (call-sites code-object)))))
   (ir code-object)))
