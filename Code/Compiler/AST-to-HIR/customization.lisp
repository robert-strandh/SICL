(cl:in-package #:sicl-ast-to-hir)

(defun maybe-wrap (client ast context call-next-method)
  (let ((origin (cleavir-cst-to-ast:origin ast))
        (dynamic-environment-location
          (cleavir-ast-to-hir:dynamic-environment-location context)))
    (if (null origin)
        (funcall call-next-method)
        (let* ((new-successors
                 (loop for successor in (cleavir-ast-to-hir:successors context)
                       collect (make-instance 'sicl-ir:breakpoint-instruction
                                 :debug-information (cdr origin)
                                 :successor successor
                                 :dynamic-environment-location
                                 dynamic-environment-location)))
               (new-context (cleavir-ast-to-hir:clone-context context :successors new-successors))
               (result (funcall call-next-method client ast new-context)))
          (if (typep result 'cleavir-ir:enter-instruction)
              result
              (make-instance 'sicl-ir:breakpoint-instruction
                :debug-information (car origin)
                :successor result
                :dynamic-environment-location
                dynamic-environment-location))))))
  
(defmethod cleavir-ast-to-hir:compile-ast :around ((client client) ast context)
  (maybe-wrap client ast context #'call-next-method))

(defmethod cleavir-ast-to-hir:compile-ast
    ;; FIXME: figure out why we are not called with a CLIENT
    ;; ((client client) (ast cleavir-ast:load-constant-ast) context)
    (client (ast cleavir-ast:load-constant-ast) context)
  ;; (format *trace-output* "**********************HERE***********~%")
  (with-accessors ((results cleavir-ast-to-hir:results)
                   (successors cleavir-ast-to-hir:successors))
      context
    (make-instance 'sicl-ir:load-constant-instruction
      :location-info (cleavir-ast:location-info ast)
      :inputs '()
      :output (first results)
      :successor (first successors))))
