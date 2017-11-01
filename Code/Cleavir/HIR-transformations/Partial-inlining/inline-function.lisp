(cl:in-package #:cleavir-partial-inlining)

(defmethod inline-function (call enter mapping)
  (let* ((*original-enter-instruction* enter)
         (initial-environment (cleavir-ir:outputs enter))
         (call-arguments
           (loop for location in initial-environment
                 for arg in (rest (cleavir-ir:inputs call))
                 for temp = (cleavir-ir:new-temporary)
                 for assign = (cleavir-ir:make-assignment-instruction arg temp)
                 do (cleavir-ir:insert-instruction-before assign call)
                    (add-to-mapping mapping location temp)
                 collect temp))
         (function-temp (cleavir-ir:new-temporary))
         (enter-successor (first (cleavir-ir:successors enter)))
         (new-enter (cleavir-ir:make-enter-instruction '() enter-successor))
         (enc (cleavir-ir:make-enclose-instruction function-temp call new-enter)))
    (cleavir-ir:insert-instruction-before enc call)
    (setf (cleavir-ir:inputs call)
          (cons function-temp call-arguments))
    (let ((worklist (list (make-instance 'worklist-item
                            :enclose-instruction enc
                            :call-instruction call
                            :enter-instruction new-enter
                            :mapping mapping))))
      (loop until (null worklist)
            do (let* ((item (pop worklist))
                      (enter (enter-instruction item))
                      (successor (first (cleavir-ir:successors enter))))
                 (setf worklist
                       (append (inline-one-instruction
                                (enclose-instruction item)
                                (call-instruction item)
                                enter
                                successor
                                (mapping item))
                               worklist)))))))

        
        
    
    
