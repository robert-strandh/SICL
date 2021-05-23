(cl:in-package #:cleavir-ast-to-hir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a STANDARD-OBJECT-P-AST.

(defmethod compile-ast (client (ast cleavir-ast:standard-object-p-ast) context)
  (assert-context ast context 0 2)
  (let ((temp (make-temp)))
    (compile-ast
     client
     (cleavir-ast:object-ast ast)
     (clone-context
      context
      :result temp
      :successor
      (make-instance 'cleavir-ir:standard-object-p-instruction
        :input temp
        :successors (successors context))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a STANDARD-OBJECT-CLASS-OF-AST

(define-compile-functional-ast
    cleavir-ast:standard-object-class-of-ast
  cleavir-ir:standard-object-class-of-instruction
  (cleavir-ast:standard-object-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a NOOK-READ-AST

(defmethod compile-ast (client (ast cleavir-ast:nook-read-ast) context)
  (let ((temp1 (make-temp))
        (temp2 (make-temp))
        (nook-number-ast (cleavir-ast:nook-number-ast ast)))
    (compile-ast
     client
     (cleavir-ast:object-ast ast)
     (clone-context
      context
      :result temp1
      :successor
      (if (typep nook-number-ast 'cleavir-ast:constant-ast)
          (let ((nook-number-input
                  (make-instance 'cleavir-ir:constant-input
                    :value (cleavir-ast:value nook-number-ast))))
            (make-instance 'cleavir-ir:nook-read-instruction
              :inputs (list temp1 nook-number-input)
              :outputs (results context)
              :successors (successors context)))
          (compile-ast
           client
           (cleavir-ast:nook-number-ast ast)
           (clone-context
            context
            :result temp2
            :successor  (make-instance 'cleavir-ir:nook-read-instruction
                          :inputs (list temp1 temp2)
                          :outputs (results context)
                          :successors (successors context)))))))))
  
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
