(cl:in-package #:cleavir-ast-to-hir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a SLOT-READ-AST

(defmethod compile-ast (client (ast cleavir-ast:slot-read-ast) context)
  (let ((temp1 (make-temp))
        (temp2 (make-temp)))
    (compile-ast
     client
     (cleavir-ast:object-ast ast)
     (clone-context
      context
      :result temp1
      :successor (compile-ast
                  client
                  (cleavir-ast:slot-number-ast ast)
                  (clone-context
                   context
                   :result temp2
                   :successor  (make-instance 'cleavir-ir:slot-read-instruction
                                 :inputs (list temp1 temp2)
                                 :outputs (results context)
                                 :successors (successors context))))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a SLOT-WRITE-AST

(defmethod compile-ast (client (ast cleavir-ast:slot-write-ast) context)
  (let ((temp1 (make-temp))
        (temp2 (make-temp))
        (temp3 (make-temp)))
    (compile-ast
     client
     (cleavir-ast:object-ast ast)
     (clone-context
      context
      :result temp1
      :successor
      (compile-ast
       client
       (cleavir-ast:slot-number-ast ast)
       (clone-context
        context
        :result temp2
        :successor
        (compile-ast
         client
         (cleavir-ast:value-ast ast)
         (clone-context
          context
          :result temp3
          :successor
          (make-instance 'cleavir-ir:slot-write-instruction
            :inputs (list temp1 temp2 temp3)
            :outputs '()
            :successors (successors context))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a NOOK-READ-AST

(defmethod compile-ast (client (ast cleavir-ast:nook-read-ast) context)
  (let ((temp1 (make-temp))
        (temp2 (make-temp)))
    (compile-ast
     client
     (cleavir-ast:object-ast ast)
     (clone-context
      context
      :result temp1
      :successor (compile-ast
                  client
                  (cleavir-ast:nook-number-ast ast)
                  (clone-context
                   context
                   :result temp2
                   :successor  (make-instance 'cleavir-ir:nook-read-instruction
                                 :inputs (list temp1 temp2)
                                 :outputs (results context)
                                 :successors (successors context))))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a NOOK-WRITE-AST

(defmethod compile-ast (client (ast cleavir-ast:nook-write-ast) context)
  (let ((temp1 (make-temp))
        (temp2 (make-temp))
        (temp3 (make-temp)))
    (compile-ast
     client
     (cleavir-ast:object-ast ast)
     (clone-context
      context
      :result temp1
      :successor
      (compile-ast
       client
       (cleavir-ast:nook-number-ast ast)
       (clone-context
        context
        :result temp2
        :successor
        (compile-ast
         client
         (cleavir-ast:value-ast ast)
         (clone-context
          context
          :result temp3
          :successor
          (make-instance 'cleavir-ir:nook-write-instruction
            :inputs (list temp1 temp2 temp3)
            :outputs '()
            :successors (successors context))))))))))
