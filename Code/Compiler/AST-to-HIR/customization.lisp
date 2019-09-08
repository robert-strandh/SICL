(cl:in-package #:sicl-ast-to-hir)

(defun maybe-wrap (client ast context call-next-method)
  (let ((origin (cleavir-cst-to-ast:origin ast))
        (dynamic-environment-location
          (cleavir-ast-to-hir:dynamic-environment-location context))
        (values-environment-location
          (cleavir-ast-to-hir:values-environment-location context)))
    (if (null origin)
        (funcall call-next-method)
        (let* ((new-successors
                 (loop for successor in (cleavir-ast-to-hir:successors context)
                       collect (make-instance 'breakpoint-instruction
                                 :debug-information (cdr origin)
                                 :successor successor
                                 :dynamic-environment-location
                                 dynamic-environment-location
                                 :values-environment-location
                                 values-environment-location)))
               (new-context (cleavir-ast-to-hir:clone-context context :successors new-successors))
               (result (funcall call-next-method client ast new-context)))
          (if (typep result 'cleavir-ir:enter-instruction)
              result
              (make-instance 'breakpoint-instruction
                :debug-information (car origin)
                :successor result
                :dynamic-environment-location
                dynamic-environment-location
                :values-environment-location
                values-environment-location))))))
  
(defmethod cleavir-ast-to-hir:compile-ast :around ((client client) ast context)
  (maybe-wrap client ast context #'call-next-method))
