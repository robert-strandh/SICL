(cl:in-package #:sicl-hir-evaluator)

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:initialize-closure-instruction)
     lexical-environment)
  (let* ((static-environment-lref (ensure-lref 'static-environment lexical-environment))
         (inputs (cleavir-ir:inputs instruction))
         (closure-lref (ensure-lref (first inputs) lexical-environment))
         (input-lrefs
           (loop for input in (rest inputs)
                 collect (ensure-lref input lexical-environment))))
    (make-thunk (client instruction lexical-environment)
      (apply (aref (lref static-environment-lref)
                   sicl-compiler:+initialize-closure-function-index+)
             (lref closure-lref)
             (loop for input-lref in input-lrefs
                   collect (lref input-lref)))
      (successor 0))))
