(cl:in-package #:sicl-ast-to-hir)

(defmethod cleavir-ast-to-hir:compile-ast
    (client (ast sicl-ast:patch-literal-ast) context)
  (let ((literal-location (cleavir-ast-to-hir:make-temp))
        (code-vector-index-location (cleavir-ast-to-hir:make-temp))
        (literals-vector-index-location (cleavir-ast-to-hir:make-temp)))
    (cleavir-ast-to-hir:compile-ast
     client
     (sicl-ast:literal-ast ast)
     (cleavir-ast-to-hir:clone-context
      context
      :result literal-location
      :successor
      (cleavir-ast-to-hir:compile-ast
       client
       (sicl-ast:code-vector-index-ast ast)
       (cleavir-ast-to-hir:clone-context
        context
        :result code-vector-index-location
        :successor
        (cleavir-ast-to-hir:compile-ast
         client
         (sicl-ast:literals-vector-index-ast ast)
         (cleavir-ast-to-hir:clone-context
          context
          :result literals-vector-index-location
          :successor
          (make-instance 'sicl-ir:patch-literal-instruction
            :inputs
            (list literal-location
                  code-vector-index-location
                  literals-vector-index-location)
            :successors (cleavir-ast-to-hir:successors context))))))))))
                     
      
   

               
