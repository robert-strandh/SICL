(cl:in-package #:cleavir-partial-inlining)

(defmethod inline-function (initial call enter mapping)
  (let* ((*original-enter-instruction* enter)
         (*location-ownerships*
           (cleavir-hir-transformations:compute-location-owners initial))
         (initial-environment (rest (cleavir-ir:outputs enter))) ; CAR is the closure vector - unneeded.
         ;; *policy* is bound closely for these bindings to make especially sure
         ;; that inlined instructions have the policy of the source function,
         ;; rather than the call.
         (call-arguments
           (loop with cleavir-ir:*policy* = (cleavir-ir:policy call)
                 for location in initial-environment
                 for arg in (rest (cleavir-ir:inputs call))
                 for temp = (cleavir-ir:new-temporary)
                 for assign = (cleavir-ir:make-assignment-instruction arg temp)
                 do (cleavir-ir:insert-instruction-before assign call)
                    (add-to-mapping mapping location temp)
                 collect temp))
         (function-temp (cleavir-ir:new-temporary))
         (new-enter (cleavir-ir:clone-instruction enter))
         (enc (let ((cleavir-ir:*policy* (cleavir-ir:policy call)))
                (cleavir-ir:make-enclose-instruction function-temp call new-enter))))
    ;; the new ENTER shares policy and successor, but has no parameters.
    (setf (cleavir-ir:lambda-list new-enter) '()
          ;; the temporary is the closure variable.
          (cleavir-ir:outputs new-enter) (list (cleavir-ir:new-temporary)))
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

        
        
    
    
