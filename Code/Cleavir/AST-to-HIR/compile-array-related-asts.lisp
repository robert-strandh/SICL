(cl:in-package #:cleavir-ast-to-hir)

;;; in case boxing semantics change
(defun box-for-type (type inputs context)
  (make-instance 'cleavir-ir:box-instruction
    :element-type type
    :inputs inputs
    :outputs (results context)
    :successors (successors context)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile an AREF-AST

(defmethod compile-ast (client (ast cleavir-ast:aref-ast) context)
  (assert-context ast context 1 1)
  (let* ((array-temp (make-temp))
         (index-temp (make-temp))
         (type (cleavir-ast:element-type ast))
         (unboxed (if (cleavir-ast:boxed-p ast)
                      (results context)
                      ;; need an additional boxing step.
                      (list (make-temp))))
         (succ (if (cleavir-ast:boxed-p ast)
                   (successors context)
                   (list (box-for-type type unboxed context)))))
    (compile-ast
     client
     (cleavir-ast:array-ast ast)
     (clone-context
      context
      :result array-temp
      :successor (compile-ast
                  client
                  (cleavir-ast:index-ast ast)
                  (clone-context
                   context
                   :result index-temp
                   :successor (make-instance 'cleavir-ir:aref-instruction
                                :element-type type
                                :simple-p (cleavir-ast:simple-p ast)
                                :boxed-p (cleavir-ast:boxed-p ast)
                                :inputs (list array-temp index-temp)
                                :outputs unboxed
                                :successors succ)))))))

(defun unbox-for-type (type input output successor)
  (make-instance 'cleavir-ir:unbox-instruction
    :element-type type
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile an ASET-AST

(defmethod compile-ast (client (ast cleavir-ast:aset-ast) context)
  (let* ((array-temp (make-temp))
         (index-temp (make-temp))
         (element-temp (make-temp))
         (type (cleavir-ast:element-type ast))
         (aset (make-instance 'cleavir-ir:aset-instruction
                 :element-type type
                 :simple-p (cleavir-ast:simple-p ast)
                 :boxed-p (cleavir-ast:boxed-p ast)
                 :inputs (list array-temp index-temp element-temp)
                 :outputs (results context)
                 :successors (successors context))))
    (compile-ast
     client
     (cleavir-ast:array-ast ast)
     (clone-context
      context
      :result array-temp
      :successor
      (compile-ast
       client
       (cleavir-ast:index-ast ast)
       (clone-context
        context
        :result index-temp
        :successor
        (compile-ast
         client
         (cleavir-ast:element-ast ast)
         (if (cleavir-ast:boxed-p ast)
             ;; simple case: no unbox required
             (clone-context
              context
              :result element-temp
              :successor  aset)
             ;; if we have to unbox the new value first, compile
             ;; the element-ast in a context where the successor
             ;; is an unboxer and the output is a different temp.
             (let ((boxed-temp (make-temp)))
               (clone-context
                context
                :result boxed-temp
                :successor (unbox-for-type type boxed-temp
                                           element-temp aset)))))))))))
